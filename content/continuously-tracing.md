最近又重新看了一下 continuously tracing 这个事情的可行性。其契机是之前又在遇到 oncall 里面，请求变慢了，又无法确认是系统中哪里导致的变慢。当前的排查手段对于特定请求的变慢还是不够准确，有监控但是监控没法对应到特定的请求，有 slow query log 但是里面记录的信息有可能遗漏或者粒度太粗而丢失了重要信息。在去年的时候，Go 官方出过一篇[博客](https://go.dev/blog/execution-traces-2024)是对 trace 做过优化，据说是做到对业务的性能影响只有 1~2%了，所以有可能做到一直开着 tracing 的程度。

最早接触和[调研 tracing](distributed-tracing.md)还是十年前，在上家公司的时候。然后在现在公司，很早期的时候给 tidb 添加过一波[tracing](use-opentracing.md)的支持。再后来 eBPF 比较火的时候也研究了一下有没有可能[不侵入 tidb 程序的情况下，去支持更强的 debug 能力](bpftrace-and-go.md)，其实结论是，就我们的使用场景来讲，手动在核心代码路径上面打点是更方便的。

tracing 发展了这么多年，就当前这个时间点，为 Go 程序添加 tracing 有这几种可能方案：

- opentelemetry
- eBPF
- Go 内置的 tracing

第一种，opentracing 已经进化成 opentelemetry 了，之前的 [repo](https://github.com/opentracing/opentracing-go) 都已经 archive 了，然后 [opentelemetry](https://opentelemetry.io/) 是新的标准，不仅有 tracing 还增加了 metrics 和 log。具体实现的库也很多，之前给 tidb 加 tracing 就是这个方案，手动在代码里面[打点](https://github.com/pingcap/tidb/blob/be7ebdb00453f616096be50ae33518ab2f8be7f7/pkg/session/session.go#L2023-L2024):

```
	r, ctx := tracing.StartRegionEx(ctx, "session.ExecuteStmt")
	defer r.End()
```

这种方式的缺点是性能影响有点大，所以我们最终并没有默认打开。倒是可以用 trace statement 来观察各阶段执行的耗时，背后的实现就是临时把单条 SQL 的 trace 打开，把 opentracing 弄到的信息打印出来，执行完 SQL 再关闭 trace。不过这个功能实际的帮助不大，因为用户很在真正在故障期间去手动地抓取信息。而一直开着则有性能影响不能接受。


```
mysql> trace select * from t5;
+----------------------------------------------+-----------------+------------+
| operation                                    | startTS         | duration   |
+----------------------------------------------+-----------------+------------+
| trace                                        | 20:48:34.871542 | 3.341667ms |
|   ├─session.ExecuteStmt                      | 20:48:34.871786 | 1.2685ms   |
|   │ ├─executor.Compile                       | 20:48:34.871809 | 759.75µs   |
|   │ │ ├─planner.Preprocess                   | 20:48:34.871815 | 13.542µs   |
|   │ │ └─planner.Optimize                     | 20:48:34.872259 | 291.417µs  |
|   │ └─session.runStmt                        | 20:48:34.872579 | 441.042µs  |
|   │   ├─TableReaderExecutor.Open             | 20:48:34.872724 | 288.417µs  |
|   │   │ └─distsql.Select                     | 20:48:34.872759 | 246.458µs  |
|   │   │   ├─copr.buildCopTasks               | 20:48:34.872767 | 212.167µs  |
|   │   │   ├─batchScanRegions                 | 20:48:34.872778 | 162.791µs  |
|   │   │   └─regionRequest.SendReqCtx         | 20:48:34.873100 | 1.0305ms   |
|   │   ├─rs.Finish                            | 20:48:34.874414 | 61.541µs   |
|   │   ├─recordSet.Finish                     | 20:48:34.874420 | 7µs        |
|   │   ├─finishStmt                           | 20:48:34.874435 | 36.041µs   |
|   │   ├─session.CommitTxn                    | 20:48:34.874442 | 19µs       |
|   │   │ └─session.doCommitWithRetry          | 20:48:34.874448 | 5.667µs    |
|   │   ├─recordSet.Close                      | 20:48:34.874484 | 393.083µs  |
|   │   ├─recordSet.Finish                     | 20:48:34.874487 | 250ns      |
|   │   ├─ExecStmt.CloseRecordSet              | 20:48:34.874495 | 377.125µs  |
|   │   ├─ExecStmt.FinishExecuteStmt           | 20:48:34.874498 | 369.042µs  |
|   │   ├─ExecStmt.LogSlowQuery                | 20:48:34.874503 | 344.125µs  |
|   │   ├─SlowQueryLogger.Warn                 | 20:48:34.874696 | 145.417µs  |
|   │   └─ExecStmt.SummaryStmt                 | 20:48:34.874855 | 625ns      |
|   ├─*executor.TableReaderExecutor.Next       | 20:48:34.873068 | 1.113084ms |
|   └─*executor.TableReaderExecutor.Next       | 20:48:34.874396 | 4.708µs    |
+----------------------------------------------+-----------------+------------+
25 rows in set (0.01 sec)
```

之前我们也有集成 web 界面，把这个信息导出到 json 然后导致到 web 界面去展示，展示上会美观一点但实际的便捷程度还不如用 trace statement 打印出来。如果 trace 的结果很大的情况下，有 web 的展示会好一点，然而在结果集很大的情况下，收集结果的过程又做得不友好，所以 web 展示是个很鸡肋的功能。

再说第二种，eBPF。这个方向主要是通过内核提供的能力去做 tracing，同时避免对业务程序的侵入。而且另外的好处是可以外界去控制开启和关闭。在 k8s 的系统上应用得很多，简单讲就是业务像黑盒一样运行在 k8s 里面，然后业务不用管怎么加上 tracing，由 k8s 那一层直接去处理掉 tracing 相关的收集。[最新的 opentelemetry 也在支持这个功能](https://github.com/alibaba/opentelemetry-go-auto-instrumentation)了，不过是免除手动插桩部分，需要特殊处理编译。对于业务很固定的情况下，其实手动去手桩也没有多大的工作量，而且当前我们已经加上了。

最后说 Go 的 trace 功能。这项功能在排查一些 runtime 层的疑难杂症真的非常有用，像[调度](go-scheduler-pitfall.md)、垃圾回收这块引起的问题，用 trace 就是最好的诊断手段。我们之前的使用姿势就是在需要的时候去抓 trace 了分析。但是不好的地方是，这个功能之前也是没办法一直开着，一方面是性能影响，另一方面是生成的文件太大了。以前一般只能抓 3-5s 的 trace 信息，文件大小就好几十MB 了，在 web 界面打开后就好几个 G。抓个 10s 以上的 trace 很可能打开就 OOM。

为什么 Go 生成的 trace 文件那么大？因为它包含的信息太多了，goroutine 在调度器的调度，channel 阻塞，进入系统调用，辅助垃圾回收，执行的耗时，阻塞之前和再次激活的时候调用栈等等等这些非常细节的信息全部会记录在 trace 文件中。因为记录的内容过于丰富，所以文件很大。在没有问题的时候总是希望，需要关注的信息越少，越直观越好。可以发生问题需要排查的时候，总是希望信息越多越好。trace 就是那种极端丰富的信息。

我看了一下新版本的 [trace 的 design](https://github.com/golang/proposal/blob/master/design/60773-execution-tracer-overhaul.md)，里面讨论的几个点主要就是：

1. 时序，因为涉及到多核，要给 trace 的 event 定序，对时间 api 的调用就跟性能很挂钩，这里涉及到取操作系统时间以及 goroutine 上面加1加1那种序号时间计算偏序
2. 面向 P 还是面向 M，之前是面向 P 的，现在改到 M，因为 M 才是直接对应于操作系统线程的
3. 切分，为了让 trace 文件能够 scale，包括分析过程能 scale，关键的点就是如何能切分成一小段一小段的
4. 格式相关的，Go 的 trace 文件目前是自定义格式，而不是按某个开放标准的格式

新的 [flight recording 功能](https://pkg.go.dev/golang.org/x/exp/trace#FlightRecorder)我试用了一下，在 tidb 中记录到慢 SQL 时，就将当时的 trace 信息写到磁盘。生成的文件还是太大，默认是 10M，虽然我调用了 SetPeriod 和 SetSize 来尝试减小文件体积，但是这似乎没有用。最后生成出来好多反而更大了，13-14M 的都有。然后这个 trace 是全局的信息，也就是那个时刻整个进程的活动都被 trace 到了，我想要再去里面寻找需要关注的慢 sql，又有点大海捞针的味道。写慢 sql 的 trace 过程还是很耗时，观察到只要涉及这里信息记录，耗时都到 1ms 以上了，毕竟这里有一个 10MB 文件的落盘。所以调研下来，感觉我们的场景下新版本的 Go trace 功能也不好一样开着做 flight recording。

调研一下子进入了僵局，正当我要放弃给出结论说不行时，柳暗花明发生在[这里](https://incident.io/blog/go-build-faster)。网上我以前读到过诊断 Go 项目的编译耗时的[博客](https://blog.howardjohn.info/posts/go-build-times/)，其中就有一项是加上 `-debug-trace` 选项，可以把 Go 编译过各的 trace 弄出来分析。那么这个 trace 是如何生成的呢？是不是用的 Go 自带的 trace? 好奇之下我去探索了下相应的[源码](https://github.com/golang/go/blob/dceb77a33676c8a4efb9c63267c351268848de6f/src/cmd/go/internal/trace/trace.go)。这是一个类似于 opentracing 那种的应用层插桩，它不是使用的 Go 自带的 trace。

我惊叹于这个实现的代码量如此之少，**只有 200 行并且完全没有任何外部依赖的情况下，就完成了应用层的 tracing 的功能**。我想到的是，把这块代码 port 进我们项目去使用。Go 自带的 tracing 的性能好，但是 trace 的内容太多了，导致文件巨大。而 opentracing 则性能较差。我完全可以上层类似于 opentracing 的应用层插桩方式，而底层按 Go 的 trace 方式去实现，基于 Go 编译器用到的 `-debug-trace` 这个代码改造一下，就能"既要又要"了。既能够性能极致的好，以至于 continuously tracing 成为可能，又要生成的文件不是过于膨胀，在需要的时候以 flight recording 的形式记录到文件。

为什么 opentracing 的性能不够好？首先它的 API 设计上就是有问题的。因为要传递 span 的父子关系信息，在业务层只有 context 的方式。而 opentracing 的 API 设计上面让 context 和 span 进行绑定了，它是：

```
func parent(ctx context.Context) {
    ctx, span = trace.StartSpan(ctx, "parent")
	defer span.Finish()

	...
	child(ctx)
}

func child(ctx context.Context) {
    ctx, span = trace.StartSpan(ctx, "parent")
	...
}
```

每次的调用都会有 `context.WithValue(ctx, tracekey, span)` 去生成一个新的 context。可别小看了这里，在 web 服务层或者更宏观的角度，这个 context 对象的创建没多少开销。但是对于数据库内部实现，加了许多打点代码的情况下，这里的 context 对象分配开销就不可忽略了。

然后 opentracing 有自己的 trace 格式定义，这些都是要符合标准规范的，不可避免就会涉及到编码解码来处理格式，又会有额外的 cpu 开销以及对象分配。再看 trace 信息的存储，我们之前用到的内存中临时存储这些 trace 信息，[它是这样的结构](https://github.com/sourcegraph/appdash/blob/e2786a622600831a80a51efea169590903c10855/store.go#L79-L84):

```
func NewMemoryStore() *MemoryStore {
	return &MemoryStore{
		trace: map[ID]*Trace{},
		span:  map[ID]map[ID]*Trace{},
	}
}
```

map 涉及了对象的分配，注定了不会得高效。

再反观 Go 的 trace 来看如何做得性能极致高效。一些关键的设计点：

- trace 的收集过程一定不要有全局的锁之类的
- zero allocation
- ring buffer
- binary 编码，自定义格式

无锁，每个线程或者这种并行的单元是完全本地完成自己的收集。event 要涉及时间，所以才会有 cputicks 和 unixnano 这些讨论。涉及到 event 定序在 Go 那边都是基于收集到的 trace 文件去做分析的时候，进行后处理修正的。

zero allocation 这个不用说，opentracing 那边就没有注意这些影响。当然，也不能怪它要那么设计 API，因为在 runtime 收集是可以直接使用到 M P G 这些信息以及 id 的，而在应用层收集就无可避免要用 context 的传递信息。

通过 ring buffer 可以一直持续收集，而是否 flush buffer 取决于业务侧是否关注某一段 trace 内容。记录 trace 跟写日志没有本质的性能差异，trace 就是在写日志，只不过记录的日志格式是事件消息。然后 ring buffer 让事件记录还是纯内存的操作，在必要的情况下可以刷盘，然后用相应的工具去分析数据。所以重的部分，解析事件和分析 trace，重新还原事件顺序，web 的展示，都是在事后去做的，而业务的 trace 过程只有最基础的事件收集。

收集事件的时候，如果只考虑这个时刻的代价低，就选择怎么简单和直接就怎么来，避免去 marshal。binary格式是最紧凑的。只有在后处理分析时，才需要去转换成标准格式，借用[三方工具](https://ui.perfetto.dev/)去展示和分析。这个时刻的数据格式是需要有标准规范的，比如 [chromium 的](https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/preview?tab=t.0)或者 opentracing 那边用到的或者 [CTF(common trace fromat)](https://github.com/efficios/ctf/blob/master/common-trace-format-specification.md) 这些。

没有理由在 Go 的 runtime 可以做到这些，到了业务层就做不到。完全可以学习 Go 的 trace 的高性能的设计，然后像 opentracing 那边在业务层插桩，基于 Go compiler trace 的最基础的代码实现，能我们的项目加上 continuously tracing 的支持。
