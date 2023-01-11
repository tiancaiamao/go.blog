Go 没有支持优先级调度，在有些场景下面，调度导致的长尾延迟是一个比较大的问题，比如说许多年以前，遇到过的[这个例子](go-scheduler-pitfall.md)。曾经觉得这种问题基本无解，作为 Go runtime 的"固有缺陷"，只能想办法绕过去。

后来同事弄的 [cpuworker](https://github.com/hnes/cpuworker) 让我看到一丝丝希望，但是深入去看又发现存在一些弊端很难克服，最好还是能够在 Go runtime 内核去解。这块工作最终停留在了 demo 阶段，哪怕 demo 非常的 promising。

直到最近，又看到 cockroachdb 的 [Rubbing control theory on the Go scheduler](https://www.cockroachlabs.com/blog/rubbing-control-theory/)，再次思考一下这个问题，发现有解！手动实现 goroutine 的调度，支持优先级，其实是可行的，只需要一个特殊的 [patch](https://github.com/golang/go/pull/51347) 能记录 goroutine 的 on-cpu 时间。

手动 goroutine 调度，只需要在项目的代码里面，插入一些 `sched.CheckPoint(ctx)` 指令。就像插入 `runtime.Gosched` 一样。执行到 `sched.CheckPoint` 这个函数后，我们可以检查 goroutine 的 on-cpu 时间已经使用了多少，比如说 20ms, 或者我们所定义的一个时间片之后，就将这个 goroutine 停下来，放到一个待调度的队列里面，等待再次被调度。

将 goroutine 停下来很容易实现，比如说通过 sync.WaitGroup，或者让它读写 channel 阻塞住。恢复执行也就是对应的 waitgroup.Done 或者 channel 操作。调度器那边，只要拿到这样的 handle，就可以控制 goroutine 的恢复执行。

调度器可以管理所有"停下来，并进入到调度队列"的 goroutine。在这里就可以支持优先级了：它可以决定先恢复哪一个 goroutine 的执行。

最终的接口很简单，代码在[这里](https://github.com/tiancaiamao/sched)：

```
func NewTaskGroup() *TaskGroup
func NewContext(ctx context.Context, tg *TaskGroup) context.Context
func ContextWithSchedInfo(ctx context.Context) context.Context

func CheckPoint(ctx context.Context)
func Scheduler()
```

我用了 task group 为单位，而不是以 goroutine 为单位进行调度管理，因为在实际使用上，task group 更合理。Go 语言很容易起一堆 goroutine，协作去完成特定的任务。有些 goroutine 生命期可能很短，而有些却又很长，goroutine 各自之间的差别很大。我们可以把这样一组 goroutine 都归到一起，成为一个 task group。

调度所需要的全部信息，都藏在 context.Context 里面了，这样 `CheckPoint` 可以直接从 context 里面通过 `sched.Key` 拿到对应的 value。task group 中的首个(主) goroutine 需要自己创建一个 `NewContext`，而后面起的属于这个 task group 的 goroutine 则是调用 `ContextWithSchedInfo`。

```
ctx1 := sched.ContextWithSchedInfo(ctx)
go subTask(ctx1)
```

一个 task group 中所有的 goroutine 的 on-cpu 时间会累记到这个 task group，当 `CheckPoint` 时发现 on-cpu 时间达到了一个时间片单位，就触发调度。实际的实现是往调度器的 channel 里面发一条消息，等待处理。

调度器收到消息后，开始执行调度操作。调度器很简单，就是一个优先级队列。**关键点在于，怎么样判定优先级？**
被送到调度器里面的 task group，它肯定是耗尽了一个时间片的，我们通过它花了久耗尽时间片来判断优先级。

假设一个时间片为 20ms 的 on-cpu 时间，比如说：

- 一个 task group，它花了 100ms (墙上)时间，来消耗掉 20ms on-cpu 时间片，那么它的 CPU 使用率是 20%
- 另一个 task group，它花了 20ms 时间，来消耗掉 20ms 的 on-cpu 时间片，它的 CPU 使用率是 100%
- 还有一个 task group，它只花 5ms，消耗掉了 20ms 的 on-cpu 时间片，它的 CPU 使用率是 400%

多核场景下的 CPU 使用率是可以超过 100% 的，取决于核数。
上面的三个 task group，显然第一个 task group 我们可以给予更高的优先级，因为它相对其它 task group 使用更少 CPU 资源，这也是 CFS(Completely Fair Scheduler) 的思想。

假设 task group A 的开始时间是 startA，进入调度队列的时间是 endA；而 task group B 的开始时间是 startB，进入调度队列的时间是 endB。当前时间是 now，那么 task group A 和 task group B 的 CPU 使用率分别是多少呢？
分别是 `20ms / [now - startA]` 和 `20ms / [now - startB]`，计算公式就是 on-cpu 时间 除以 经过的墙上时间。因为进入调度队列之后的 `[endA~now]` 和 `[endB~now]` 那一段时间是不算 on-cpu 时间的，并且分子 20ms 时间片参数相对于所有 task group 是固定的。

这样观察可以发现，最终决定优先级的参数只需要开始时间，也就是 startA 和 startB。开始时间越早，进入调度队列后计算得到的 CPU 使用率越低，所以调度的权重越高，越应该被优先调度。

## 答疑部分

### 调度用了一个优先级队列。如何决定谁的优先级高？

前面已有描述

### 所以说白了，就是手动调用 CheckPoint 计算工作量决定是否将 goroutine hold，那啥时候再 yield 回来呢？

由调度器的调度决定。

如果调度器只是简单地将待调度队列里面的任务，**按优先级次序全部出队列(恢复执行)**，会有问题：这个队列堆积不起来，同时，CPU 也限制不住。
CPU 是一种资源，资源的总量是有限的。我们需要控制这种资源的使用量，然后做排队。通过排队的优先级处理，让优先级更高的任务获取资源(得到响应)。
和常规的调度不太一样的地方：我们只有决定 CPU 的使用量，才能有效地控制排队，否则 goroutine 并不会 block 在调度队列里边。

### 如何决定 CPU 的使用量？

CPU 的使用率等于 on-cpu 时间 / 墙上时间。如果把 on-cpu 时间看成一种资源，我们可以对这种资源进行 rate limiter，从而控制资源的使用，也就是 CPU 的使用量。
rate limiter 可以用普通的 token bucket 算法。

### 用 token bucket algorithm 决定 CPU 的使用量。那么如何决定 token 的生成速率？

假设总共有 10 个核，则总的 CPU 资源为 1000%。如果将 CPU 使用率限制在 80%，则总的 CPU 使用率为 800%。
即 on-cpu 时间 / 墙上时间 = 800%，这也正是对应 "token" 的生成速率：

- 每经过 1000ms，可以恢复 800ms 的 on-cpu 时间资源
- 每经过 100ms，可以恢复 80ms 的 on-cpu 时间资源
- 每经过 10ms，可以恢复 8ms 的 on-cpu 时间资源

假设漏桶的桶大小 capacity = 100ms 的 on-cpu 时间，那么如果剩余的 tokens 多于 20ms，则可以恢复调度队列中的一个任务的执行，否则要等待 token refill。

### 为什么是 task group 而不是以 goroutine 为调度单位？

因为 goroutine 很多是短暂生命的，不同 goroutine 的使用目的不同，并不等价。用 goroutine 不方便管理。

### 怎么算时间片？

暂时还没有做调优，先假定为 20ms 吧... 这个设置越小，调度的越频繁，实时性就越好，但调度本身的开销越多。

### 跟 goroutine pool 或者 worker pool 有什么区别？

网上的 goroutine pool 实现铺天盖地，多如牛毛。比如这里随便列举一些：

- https://github.com/alitto/pond
- http://marcio.io/2015/07/handling-1-million-requests-per-minute-with-golang/
- https://brandur.org/go-worker-pool
- https://gobyexample.com/worker-pools
- https://github.com/panjf2000/ants
- https://github.com/gammazero/workerpool

这个库并不是 yet another worker pool。

上面的这些 pool 解决什么问题？主要是解决 goroutine 超频繁分配释放的问题。
要不然就是想解决 goroutine 数量实在太多之后，引起的 scheduling 性能问题。

基本上这些池子的实现，都是 *用 M 个 goroutine 去驱动 N 个任务，这种做法在通用性上面有很大的问题*，使用场景是受限的。
只能用于纯 CPU 的任务。这些实现有一个算一个，都是个雏儿。不信？我们看个 [ManOrBoy 测试](https://github.com/tiancaiamao/gp/blob/35bb0a548e2404a504a7c2392e9e7244065222fc/gp_test.go#L73-L136)。

假设用池子里面的 goroutine 执行用户逻辑，用户代码 panic 之后，是不是池子的 worker 就少了一个？panic 很多次之后，是不是没有 worker 干活了？

用户代码 sleep 之后，worker 数量是不是对应的少了一个？如果 worker 数量减得没有了，再往 pool 发送任务是不是得不到执行了？

block 之后，也是同样的道理。用 M 个 goroutine 驱动 N 个任务，这种模式就是没法应对业务代码执行到 block 的，它会 block 住 M 个 goroutine 的一个，让能干活的人越来越少，最终整个 pool 不可用。

### 跟我自己的 gp 有什么区别？

[gp](https://github.com/tiancaiamao/gp) 是我写的一个简单的 goroutine pool，它主要是解决 goroutine 栈分裂的场景的开销，本意并不是解决 goroutine 超频繁分配释放，也不是要减少 goroutine 数量。
所以... 它是可以执行任何用户代码逻辑的。

### 跟 CPU worker 有什么区别？

[CPU worker](https://github.com/hnes/cpuworker) 是我同事之前写的一个库，目的跟这儿都一样是 "A Customized Goroutine Scheduler over Golang Runtime"。这个 sched 库可以说是 inspired by 它，也 inspire by 一些其它的东西。

CPU worker 中，用了固定数量的 goroutine 去驱动不固定数据的 task，并且预留了一些 CPU，从而保证 CPU 使用率最高只到 XX%。
预留出 CPU 资源保证了 Go 的调度能力。

缺点是 CPU worker 没有考虑到网络等 block 的场景，一旦涉及复杂的交互，就不 work 了。凡是用 M 个 goroutine 去驱动 N 个任务的实现方式，都是这样的缺陷。
只能 demo 一下 idea 可行。失去了跟原生代码交互的能力！！！

### 跟 conc 有什么区别？

[conc](https://github.com/sourcegraph/conc) 是一个结构化并发的库，主要是提供更 high-level 的安全的并发原语，跟这个手动 goroutine 调度完全不是一类东西，并没啥可比性。

### 必须依赖给 Go 打 patch 么？

是的。实现强烈依赖获取 goroutine on-cpu 时间的能力。

如果我们用 pool 的方式去实现，也可以追踪到执行的任务的耗时的情况，并提供一定的任务"调度"的能力。但是由于用户逻辑可能有网络，有 block 的情况，用墙上时间来衡量资源使用就不准确了。

这个 [patch](https://github.com/golang/go/pull/51347) 的改动量很小，寄希望 Go 官方能合入吧。

### 这个手动 goroutine 调度器的实现，跟 cockroachdb 的那个实现有什么区别？

cockroachdb 那边把许多逻辑放到一起了，叫 admission control，不光处理了 CPU，还有 IO 等其它资源。
它那边叫流控。本质上，流控和调度等价，当然，这是额外的话题了。

cockroachdb 里面，流控会为 goroutine 分配 CPU 的时间片，按一定的速率生成 token。当 goroutine 执行到某个检查点，会检查 goroutine 的时间片是否耗尽。如果是，则 goroutine 卡住在 channel 上面，等待 token 往这个 channel 填充。

反馈机制跟这个实现也不太一样。它的反馈机制是，根据 runtime scheduling 的 latency，来动态调整时间片的大小。如果系统繁忙了，runtime 的调度 latency 会提高，然后反馈机制会调小时间片，那么 goroutine 在检查点执行更少的任务就会卡住，等待 token refill。goroutine 能执行的时间就变少了，block 住的时间增加了，goroutine 慢下来了。这就是一个典型的流控模式的反馈：让参与者全部"都"慢下来。

我写的这个调度器是调度，反馈机制是 CFS，会根据 CPU 资源使用计算优先级，会让消耗 CPU 资源更高的，更不容易被执行，从而慢下来。

### 这个库可以用了么？

还不行吧，目前还在 demo 阶段，我验证了一下 cpu worker 那边的 case，确认了做手动调度是有效的。通过这个调度之后，可以让大的费 CPU 的请求，不影响调度延迟，避免小的请求的长尾问题。
距离真正能用还需要更多的测试和打磨。
