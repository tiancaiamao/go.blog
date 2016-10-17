给系统打压力，内存占用上去了，停止打压后，仍然降不下来，就可能是有泄漏。

对于无状态的服务，连接上有请求过来，内存上去了。停了请求，但是内存仍然居高不下，等到连接断开内存才降，则session可能存在不合理的引用，造成GC无法回收。

看内存占用的时候不要光盯着top上面的数字，因为Go向系统申请的内存不使用后，也不会立刻归还给系统。也就意味着停止打压后内存占用不立刻下降属于正常行为，仅看这一项需要多花点时间观察一会儿。最好是几个数据一起关注：一个是程序占用的系统内存，另一个是Go的堆内存，最后一个是实际使用到的内存。从系统申请的内存会在Go的内存池管理，整块的内存页，长时间不被访问并满足一定条件后，才归还给操作系统。又因为有GC，堆内存也不能代表内存占用，清理过之后剩下的，才是实际使用的内存。调用`runtime.ReadMemStats`可以看到Go的内存使用信息; 或者启用net/pprof后访问 http://127.0.0.1:6060/debug/pprof/heap ，也可以看，其中HeapInuse是实际的内存使用量，具有参考意义; 还可以带上参数[debug/pprof/heap?debug=2](https://github.com/golang/go/blob/master/src/runtime/pprof/pprof.go#L284)之类得到更细的信息，

发现问题后，首先不要怀疑Go语言的GC有问题，一定要相信是自己的代码写的有问题。内存释放不掉，不是泄漏的，而是代码肯定有什么地方还引用着那块内存，导致GC无法释放。怎么查问题呢？Go语言有pprof这个神器。

经过在上一步看HeapInuse，确认过内存没释放之后，可以用 `go tool pprof -inuse_space http://127.0.0.1:6060/debug/pprof/heap` 查看是什么地方占用了内存，这里可以大致看到是什么函数占内存，根据代码可以推测出一些信息。一般来讲，很有可能的就是goroutine leak，然后里面引用到的内存都释放不掉。举一个例子：

    ch := make(chan T)
    go produce(ch) {
      // 生产者往ch里写数据
      ch <- T{}
    }
    go consume(ch) {
      // 消费者从ch里读出数据
      <-ch
      err := doSomeThing()
    }
    
消费者发生err并退出了，不再读ch，导致生产者阻塞在`ch <- T{}`上面，然后生产者的goroutine就泄漏了，里面引用的内存永远无法释放。这是一个十分常见的场景，比如说开多个worker，worker处理好数据后写channel，主线程读channel，但是主线程处理过程中出错退出，如果处理不当worker就可能泄漏的。

开启net/pprof之后，通过 http://127.0.0.1:6060/debug/pprof/goroutine?debug=1 可以看到当前的所有goroutine栈，可以找到有哪些goroutine，当前执行到什么位置，可以找到在哪里goroutine泄漏了。

上面说了如何分析和定位Go程序的内存泄漏问题，接下来讲一下如果避免写会泄漏的代码。

第一条原则是，绝对不能由消费者关channel，因为向关闭的channel写数据会panic。正确的姿势是生产者写完所有数据后，关闭channel，消费者负责消费完channel里面的全部数据：

    func produce(ch chan<- T) {
        defer close(ch) // 生产者写完数据关闭channel
        ch <- T{}
    }
    func consume(ch <-chan T) {
        for _ = range ch { // 消费者用for-range读完里面所有数据
        }
    }
    ch := make(chan T)
    go produce(ch)
    consume(ch)

为什么consume要读完channel里面所有数据？因为`go produce()`可能有多个，这样写的代码，在读完ch可以确定所有produce的goroutine都退出了，不会泄漏。

第二条原则是，利用关闭channel来广播取消动作。向关闭的channel读数据永远不会阻塞，这是进阶的技巧。假设消费者拿到数据处理后有error发生，整个动作失败，那么需要有某种机制通知生产者停止并退出。

    func produce(ch chan<- T, cancel chan struct{}) {
        select {
          case ch <- T{}:
          case <- cancel: // 用select同时监听cancel动作
        }
    }
    func consume(ch <-chan T, cancel chan struct{}) {
        v := <-ch
        err := doSomeThing(v)
        if err != nil {
            close(cancel) // 能够通知所有produce退出
            return
        }
    }
    for i:=0; i<10; i++ {
        go produce()
    }
    consume()

WaitGroup之类的可以配合着用，看自己喜欢的风格。基本上能处理好error场景下的资源释放，问题就不大。哦，第零条原则是，对于并发的代码心存敬畏之心，哪怕用Go，哪怕有channel这么好用的东西！

[context里面的cancel](https://godoc.org/context#WithCancel)比较值得参考和学习 ，其实没什么技巧，就是多看代码多写代码，标准库的代码是极好的学习材料。
