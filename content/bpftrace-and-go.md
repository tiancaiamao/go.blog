ebpf 是 linux 内核提供的一个可以观察代码执行的技术。不仅可以观测内核，还可以观测用户层的执行。

bpftrace / bcc 这些是围绕 ebpf 实现的一些具体的工具。
bcc 是 python 接口的，libbpf 是纯 C 的。Go 语言这边封装的库有 clium 的 [ebpf](https://github.com/cilium/ebpf)，这个是纯 Go 的。然后 [gobpf](https://github.com/iovisor/gobpf) 是用 cgo 封装的 libbpf 提供的一个 Go 的库。

最简单的教程，建议从 [Debugging with eBPF](https://blog.px.dev/ebpf-function-tracing/) 系列学起，一共三篇博客，读完就对原理了解了个大概。

然后就是工具选择，bcc 或者 ebpf 这种库的还是使用起来比较麻烦，而 bpftrace 这个工具，学习成本则非常的低，易于上手，推荐这个。

bpftrace 直接看官方的 [reference guide](https://github.com/iovisor/bpftrace/blob/master/docs/reference_guide.md#9-1--n--positional-parameters) 或者 [one liner tutorial](https://github.com/iovisor/bpftrace/blob/master/docs/tutorial_one_liners.md) 都行，文档特别简洁，跟着上手走一篇就掌握了。

好了，进入正题，用 bpftrace 观察 Go 程序。由于 Go 的调用协议跟 C 不太一样，直接使用时会遇到两个问题：

- uretprobe 会导致被观测程序 panic
- 默认没有追踪 goroutine 的方式

先看第一个，比如合 one-liner 里面的统计函数调用 latency 的例子改一改:

```
bpftrace -e 'uprobe:myproc:func { @start[tid] = nsecs; } uretprobe:myproc:func /@start[tid]/ { @ns[comm] = hist(nsecs - @start[tid]); delete(@start[tid]); }'
```

在 uretprobe 的时候就会出现 panic 了，问题见这个 [issue](https://github.com/golang/go/issues/22008)。这个原因是 uretprobe 需要 hack 处理函数返回，trampoline 到 bpf 的代码执行后再跳回正常的返回，而 Go 的协议里面用了 split stack，并不是正常的 C 那样的返回协议了，所以 uretprobe 之后就出问题了。
这个目前暂时无解，处理方式就是，不使用 uretprobe。

绕过去的方式可以一直用 uprobe，先 uprobe 函数的入口地址，然后 uprobe 函数 +offset 的位置，一直到 ret 指令的地方。
获取函数位置有几种方法：

- gdb 或者 dlv 连上去调试，打印出函数地址
- objdump -d binary 或者 objdump -t binary 输出 binary 的信息，grep 对应的符号找到地址
- bpftrace -lv 'uprobe:binary:xxx' 星号的方式把查找函数符号信息


但是似乎使用 +offset 的写法也还是不行：

```
bpftrace -e 'u:./tidb-server:"github.com/pingcap/tidb/server.(*clientConn).handleStmtExecute"+2919 {printf("hello")}'
Attaching 1 probe...
Can't check if uprobe is in proper place (compiled without (k|u)probe offset support): ./tidb-server:github.com/pingcap/tidb/server.(*clientConn).handleStmtExecute+2919
```

不过倒是可以用 uprobe 把进入 defer 函数调用的位置当作 uretprobe 使用：

```
func f() {
	defer func() {
		_ = 42
	}
	// ...
}
```


```
bpftrace -e 'uprobe:myproc:f { @start[tid] = nsecs; } uprobe:myproc:f.func1 /@start[tid]/ { @ns[comm] = hist(nsecs - @start[tid]); delete(@start[tid]); }'
```

使用 bpftrace 观察 Go 程序的第二个问题，bpftrace 默认只提供以 pid / tid，也就是线程的观测。而对于 Go 来说，是多个 goroutine 共享线程的，比如我要写一个观察函数 latency 的代码，用 `@latency[tid]` 这种是不行的，goroutine 会切换来切换去。
需要有一个类似 `@latency[gid]` 的东西。但是 gid 被有暴露出来，所以就不好处理 goroutine 级别的 tracing 了。

网络找了找，发现了一个 [workaround](https://gist.github.com/felixge/7b21f8c3fc3add7a2a3e52dbac65cc53#file-trace-bt)。这个代码段里面，goroutine 切换会调到 `runtime.execute`，所以它在 `runtime.execute` 的时候，把 gid 记录到全局对象 `@gids[tid]` 里面，这样 thread 和 goroutine 的映射关系就被记录下来了。
需要的时候，使用 `$gid = @gids[tid];` 就可以获取到 gid。

有了这些，就可以写一个观察 tidb 的一条请求处理耗时了：

```
cat latency.bt

BEGIN {
  printf("Hit CTRL+C to end profiling\n");
}


uprobe:/home/genius/project/src/github.com/pingcap/tidb/bin/tidb-server:runtime.execute {
	// map thread id to goroutine id
	@gids[tid] = reg("ax")
}

tracepoint:sched:sched_process_exit {
  delete(@gids[tid]);
}




uprobe:/home/genius/project/src/github.com/pingcap/tidb/bin/tidb-server:"github.com/pingcap/tidb/server.(*clientConn).handleStmtExecute" {
	$gid = @gids[tid];
	@start0[$gid, pid] = nsecs;
}

uprobe:/home/genius/project/src/github.com/pingcap/tidb/bin/tidb-server:"github.com/pingcap/tidb/server.(*clientConn).handleStmtExecute.func1" {
	$gid = @gids[tid];
	if (@start0[$gid, pid]) {
		printf("gid=%d tid=%d pid=%d...takes %d us\n", $gid, tid, pid, (nsecs - @start0[$gid, pid]) / 1000);
		@durations["dispatch"] = hist((nsecs - @start0[$gid, pid])/1000);
	}
}

uprobe:/home/genius/project/src/github.com/pingcap/tidb/bin/tidb-server:0x1732d60 {
	$gid = @gids[tid];
	if (@start0[$gid, pid]) {
		printf("gid=%d tid=%d pid=%d...takes %d us\n", $gid, tid, pid, (nsecs - @start0[$gid, pid]) / 1000);
		@durations["tso"] = hist((nsecs - @start0[$gid, pid])/1000);
		delete(@start0[$gid, pid]);
	}
}
```

运行：

```
bpftrace ./latency.bt
Attaching 6 probes...
Hit CTRL+C to end profiling
^C

@durations[dispatch]: 
[0]                   24 |@                                                   |
[1]                   53 |@@@                                                 |
[2, 4)                18 |@                                                   |
[4, 8)                 1 |                                                    |
[8, 16)                1 |                                                    |
[16, 32)               0 |                                                    |
[32, 64)               0 |                                                    |
[64, 128)              0 |                                                    |
[128, 256)           242 |@@@@@@@@@@@@@@                                      |
[256, 512)           893 |@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@|
[512, 1K)            155 |@@@@@@@@@                                           |
[1K, 2K)              29 |@                                                   |
[2K, 4K)               8 |                                                    |
[4K, 8K)               0 |                                                    |
[8K, 16K)              0 |                                                    |
[16K, 32K)             0 |                                                    |
[32K, 64K)             6 |                                                    |
```

注意 latency.bt 里面的那个处理 gid 的方式并不对，不过没关系，这个场景只要能区分出来 goroutine，并不一定需要准确的 goroutine id。

基础的使用就这些了，后面就是去探索无限的可能性，靠发挥想象力，比如去研究请求的长尾延迟到底受到了 Go runtime 的何种影响。
因为 ebpf 是可以不侵入改应用的情况下，收集这些信息的，还可以知道函数调用参数之类，确实是有很大的想象空间。
