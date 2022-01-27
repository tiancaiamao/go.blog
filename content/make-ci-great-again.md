## 问题描述

其实在这个 [issue](https://github.com/pingcap/tidb/issues/30822) 里面，我已经描述过了。

TiDB 的 CI 不稳定，不稳定的来源一个是环境(次要因素)，一个是测试 case 本身 (主要因素)。**并发的姿势不对，是测试 case 不稳定的根源问题**。

旧的测试框架下，test suite 之间是并行的，同一个 test suite 内部是各个 test case 串行的

我们在[重构测试代码](https://github.com/pingcap/tidb/issues/30822)，新的代码中，测试是否并发取决于测试 case 在函数开头是否调用 `t.Parallel()`

不管旧的代码还是新的代码，当前的并发方式，都是一个 package 一个进程，在 package 内部测试并发，**多个 case 跑在同一个进程，代码容易相互影响，导致 CI 不稳定**：

- 比如设置过 failpoint 影响其它测试
- 比如配置的修改影响到其它测试
- 比如代码里面有些全局变量之类的，多个测试在单个进程内跑无法隔离

还有一种情况，是**并发压力的影响，CI 环境压力不大的时候没事，当环境压力很大之后就不稳定**。

测试 case 本身写得不好是一方面，而环境对并发度的不可控也对 CI 稳定造成很大困扰，最终后果都是我们花大量的成本在修复 CI。

修测试的成本是很高的，有些测试在本地环境跑得好好的，但是在 CI 环境就有时候会跪，而复现又困难。受环境影响而挂掉的，这些都特别难排查，加 log 去 CI 上面打印，改一次跑一次的周期特别长。

还有一种 panic 超时的，就是某个测试里面有 panic 了，或者后台 goroutine 啥的，而测试逻辑又在等待那个结果，永远等不到，测试就卡住了。
最后是超过 10min 才报错退出。

本来嘛，加单个测试的超时机制，应该能解决掉这个问题。然而加超时机制又引入另外的坑，false negnative... 因为 CI 环境机器压力大，有时候跑得一慢，就误报超时了，然后又不稳定。 

## 解决方案

初步的解决方案，换一种并发方式，**让每一个测试 case 跑在一个单独的进程里面**

对于旧的代码，测试 case 的执行调用

```
go test -test.run '^TestT$' TestXXX
```

对于新的代码，测试 case 的执行调用

```
go test -test.run TestXXX
```

这种执行方式就解决单个进程内，测试之间相互影响导致不稳定的问题

接下来是并发控制部分，**由外部去控制同时跑多少个测试进程**

这个 [PR](https://github.com/pingcap/tidb/pull/30828) 中做了一个工具，用一个进程跑一个测试，单个测试内部不再并行; 根据环境 CPU 的数量，决定同时跑多少个进程，可以兼顾到并发，又不让环境压力过大导致 CI 不稳定

使用方式：

	make ut // 跑所有测试
	make ut X='list' // 列举有哪些包
	make ut X='list session' // 列举 session 包下有哪些 test case
	make ut X='run planner/core' // 并行执行 planner/core 包下的所有 case
	make ut X='run util/ranger TestTableRange' // 执行 util/ranger 包的单个测试函数

## 具体实施

我列了一个工作计划：

- [x] 去掉代码中旧的并行方式 t.Parallel()
- [x] 提供并行执行 ut 的工具
- [x] 处理 coverage 等问题
- [ ] 处理单个测试的超时限制
- [x] 替换默认的 CI pipeline

然后记录下这一路克服的障碍。

### 如何有效地并发

看了一下当前的 test case 数量，在 tidb 仓库内的单测当前大概是 4700+ 个。如果串行跑太慢了...

想到的首先是弄几个 worker，有多少个 numa 核，就起多少个 worker，然后把包的所有测试往 worker 发送。但是发现 CPU 压力有点太高了...查了一下发现是 `go test -run` 的时候，每个进程都假设自己是可以使用全部 GOMAXPROC 的，那么跟 worker 数叠加之后，并发就超载了。解决这个可以通过参数 `-test.cpu` 指定使用的 CPU 数量。

按包的粒度的拆分不是特别合理，包与包之间的差异特别大。比如 executor, planner/core 这些是相对比较大的包，而 util/hack 这种包很小。一些要跑挺久，一些瞬间就执行完了。所以改成不管包的大小，把所有包的所有测试函数收集起来，然后用函数粒度去分任务。

然后是做 shuffle，有些函数可以吃满 CPU，比如 expression 包下面的各种函数，而有些测试 case 里面则的很多的 sleep。做 shuffle 让它们的分布均匀随机一些。

我又发现 CPU 用不满。我们写的测试，单个测试用例，基本上都是单线程逻辑的，如果给到多个 GOMAXPROC 去执行一个测试，其实只会用到一个 CPU 那么多的资源。最后我干脆就 `-test.cpu=1` 去跑每个测试，而进程数使用等于 cpu 的数量。

### 处理失败的 case

即便当前是每个测试函数跑一个进程的，并发起来的时候，还是有一些可能失败。我发现很典型的是代码里面写死了 /tmp/xxx 文件，
多个测试同时跑着，结果发现自己的 /tmp/xxx 文件没了... 可能被其它的测试用例删除了。

有也测试是自身的稳定性，即使拿出来单独跟，也有小概率地失败，这种 case 需要针对地修理。

### numactl 的坑

开始时我用了 `numactl -- xxx.test -test.run` 这样的命令去执行测试，调试时发现有一些神奇的报错发生，有 SIGSEGV 和 SIGTRAP 一类的信号，导致测试进程退出了。我就很纳闷，SIGSEGV 我查不到 core dump，也没看到 Go 那边的退出的栈，设置 GOTRACEBACK 变量也没用...至于  SIGTRAP 就更费解了，啥时候跑 Go 的测试会收到这个信号而退出？

后来明白了，应该不是测试自己的，而是 numactl 给它发的，而测试中肯定不会处理这些信号，就导致了退出。干脆不要做 numactl 绑核，只是 `-test.cpu=1`

### 生成测试结果报告

之前的 CI pipeline 会把测试结果生成一个 JUnitFile 了上传，研究了一下之前的脚本后发现它是用了 [gotestsum](https://github.com/gotestyourself/gotestsum) 这个包的功能。"JUnit XML for CI integration, and a summary of the test results"

```
gotestsum --junitfile unit-tests.xml
```

这个功能其实挺简单的，我可以把[文件格式](https://github.com/gotestyourself/gotestsum/blob/661b09182c5e919dfb1d14994f67578f9797164e/internal/junitxml/report.go#L19)拿到，生成对应的格式就行了。

### coverage

如果是跑单个 package 的测试，`go test -coverprofile xxx` 可以 coverage 文件。然而新的测试的用户都是每个测试函数单独跑的，虽然也可以生成出来 coverage 文件，但生成的 coverage 就不准确了，只包含了单个测试函数覆盖的。`go test -run TestXXX -coverprofile xxx`

于是去研究了一下 coverage 文件的格式。其实格式也挺简单的，大概长这种样子：

```
mode: set
github.com/pingcap/tidb/server/http_status.go:61.36,63.2 1 1
github.com/pingcap/tidb/server/http_status.go:65.64,72.2 6 0
github.com/pingcap/tidb/server/http_status.go:74.57,75.9 1 0
github.com/pingcap/tidb/server/http_status.go:76.23,76.23 0 0
github.com/pingcap/tidb/server/http_status.go:77.20,77.20 0 0
github.com/pingcap/tidb/server/http_status.go:81.49,83.50 2 1
github.com/pingcap/tidb/server/http_status.go:87.2,90.16 4 1
github.com/pingcap/tidb/server/http_status.go:94.2,96.22 2 1
github.com/pingcap/tidb/server/http_status.go:102.2,102.16 1 1
```

其中第一行是 `mode: set`，这个 mode 可以是 set, atomic, count ... 由编译参数  -covermode 决定。
剩下的每一行，是 name.go:line.column,line.column numberOfStatements count
源文件是在[这儿](https://github.com/golang/go/blob/0104a31b8fbcbe52728a08867b26415d282c35d2/src/cmd/cover/profile.go#L56)

然后我尝试了一下，把包下面的所有测试生成的文件，合并成一个之后，仍然是有效的。

`go test -cover -func=xxx.out` 可以验证...于是生成 coverage 这个问题就解决了。

### 加速编译

刚开始，为了编译每一个包的 test binary，我是遍历整个 package，然后依次 `cd pkg; go test -c` 的方式弄的。然后发现，这种方式只能形容它是 painfully slow。

首次的要下载依赖包，编译缓存等，速度最慢可能搞 20min 以上...但是后面的编译，一个包一个包的做法，用不满 CPU。观察发现 Go 编译器的 linking 过程还是单 CPU 的，用不上多核。

标准的编译方式 `go test ./...` 这种，它可以一次把多个包编译出来，速度还挺快，编译和运行是一起的。目前 Go 还没有支持并行编译全部的测试包，但不执行。

google 了一下，找了这里这个[黑科技](https://github.com/golang/go/issues/15513#issuecomment-773994959)

用 `go test --exec xprog`，它会使用 xprog 程序去调用编译好的测试包，我只需要传一个自己的 xprog 程序，就可以控制测试的执行，perfect!

实际 xprog 接收到的第一个参数，就是编译过程中生成的 xxx.test 文件，一般是在路径 /tmp/go-build3146868117/b1742/xxx.test 下，同目录还有一个 importcfg.link 文件，从里面可以解析出包路径之类的信息。
