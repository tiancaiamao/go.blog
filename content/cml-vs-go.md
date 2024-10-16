咋看一眼，CML(concurrent ml) 并发模型似乎是 Go 的一个子集: CML 的 channel 是不带缓存的，而 Go 的 channel 可带缓存可不带。
仔细学习之后，发现两者并发模型并不相同，CML 也不算 Go 的子集，而是一个"表达能力"几乎更强的并发模型。

Go 的并发模型就不做介绍了，假设读者都知道。反正只要写过 goroutine + channel 很快就明白是怎么回事。

CML 这边，第一个差异点是 CML 的 channel 是不带缓存的。其原因本质上是 Go 和 CML 对于通信的认知的不同。根据 Rob Pike 的名言 "Don't communicate by sharing memory; share memory by communicating" 可以知道，在 Go 里面 communicate 是用来 share memory 的，而 channel 带缓存，就是这种 share memory 的具体表现。在 CML 里面，**channel 的两个 coroutine 之前的交汇点**，这个"交汇点"就是字面意思的两个 coroutine 相交的时刻，并不是设计成两个 coroutine 之间的 share memory by communicating，所以它只是一个值，不带缓存。

第二个差异点，或者说 CML 和 Go 更深层次的差异： 在 Go 的设计 channel 是 first-class 的。而**在 CML 的设计中 event 才是 first-class 的**。

在 Go 里面 "channel 是 first-class (一等公民) 的"，怎么样理解呢，除了 `select` 和 `<-` `->` 几个操作可以访问 channel 之外，channel 可以作为函数的参数，可以作为函数的返回值，甚至 channel 里面还可以传 channel，所以它是一等公民，这里一个小例子：

```
ch := make(chan chan struct{}) // channel 的 channel

go f() {
    done := make(chan struct{})
    ch <- done // 任务发送给另一个 goroutine g 去执行

    <-done // 注意任务执行完的通知方式，是传了一个 chan 过去，对方会利用这个 chan 回传信息
}

go g() {
    for task := range ch {
        // do something with this task
        close(task) 
    }
}
```

CML 则不同，event 才是 first-class 的。CML 模型更像是一套协议，所有符合条件的并发阻塞的场景都可以设置成 event。
比如说读取 channel 是一个 event，读网络或者 io 操作同样也可以设计成一个 event，甚至 sleep / timer / ticker 等等这些都可以是做成 event。
(recv ch) 生成一个读取 channel 的 event。(read fd) 生成一个文件 io 的 event。

sync 原语用于执行一个 event。比如在 Go 语言里面的 `v := <-ch` 对应到 CML 里面，就是 (sync (recv ch))。那么怎么样表达 Go 里面的 select 语义呢？ choice 用于组个多个 event，返回一个新的 event。新的 event 执行会打乱被组合的 events 之后顺序尝试，如果尝试到其中某个成功则结束，否则一直等待到其中最早的一个 event 被触发，返回其执行结果。

```
(sync (choice (recv ch1) (recv ch2)))
```

等价于 Go 里面的

```
select {
    case <-ch1:
	case <-ch2:
}
```

**event 是可组合的(composable)**，这导致了更高的表达能力，choice 是一个用于组合 event 的原语。怎么样表达 Go 里面带 default 的 select 呢? 很简单，可以引入 always 和 never 这样的 event，然后用 choice 去组合。比如说

```
(sync (choice (shuffle (recv ch1) (recv ch2)) always))
```

`(warp event f)` 会得到一个新的 event，当它被 sync 的时候执行结束时，f 会调用执行的结果生成一个新的结果。比如:

```
(sync (send ch "hello")) // 其它 coroutine 中执行这个

(sync (wrap (recv ch)
            (lambda (v)
                (append v " world")))) // 会得到 "hello world"
```

好啦！是时候来证明 CML 的并发模型 "表达能力" 比 Go 更强啦！

Go 语言可以表达这样的 select 不? 同时读取 channel 和执行网络 io，哪一个先到则执行哪一个？ CML 可以。因为 (recv ch) 和 (net.read fd) 都是 event，它们是可组合的：

```
(sync (choice (recv ch) (net.read fd)))
```

Go 语言里面可以表达以 select 的方式读取 channel 的数组不？ channel 的数量不定 (或许可以通过 reflect 之类的 hack 实现)，比如 

```
func f(ch ([]chan struct{}) {
    select ch...?
}
```

CML 可以。

```
(lambda (chs)
	(sync (choice (map (make-recv-op) chs))))
```

这些都是在 Go 里面不能表达的，而 CML 中可以表达的。前面说过 CML 和 Go 有一个差异是，CML 中 channel 不带缓存。其实带缓存的 channel 可以用不带缓存的 channel 来模拟，只需要额外用一个 coroutine，不停地将 chan 的读写结果先缓存在一个队列中。

```
(let ch (chan.make)
     (spawn (lambda ()
               (let-loop recur (i 0)
					(enqueue q (sync (recv ch)))))
					(if (> i 10) break
					    (recur (+ i 1))))
     ... ;; 这里就等价于 ch 是带了 10 个缓存的 chan 了
```

所以 CML 的并发模型的表达能力是大于等于 Go 的。Go 能表达的它都能表达，而它能表达的 Go 都不一定能。

CML 的问题是什么呢？问题是太小众啦，它的知名度远远对不上它么牛B的能力...确实是值得更多曝光的。
如果想体验一下，不一定非要使用 SML/OCaml 之类的，也可以考虑一下 cora，最近[几次](https://github.com/tiancaiamao/cora/blob/0b6d959f627ee89425d781aa7e5a196f2fb2eae2/lib/async.cora#L116-L159) [提交](https://github.com/tiancaiamao/cora/blob/0b6d959f627ee89425d781aa7e5a196f2fb2eae2/test/benchmark/ping-pong.cora#L10-L40) 把 channel 和异步 io 都实现了，并发模型就是基于 CML 的。


