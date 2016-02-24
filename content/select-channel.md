先看这样一个需求：Go语言中，对`chs []<-chan struct{}`进行select操作，如果数组里所有channel都不可读，就阻塞调用的goroutine。

似乎是没有什么直接的方式，比如这样是不行的：

    for _, c := range chs {
        select {
            case <-c:
                succ = true
                break
            default:
        }
    }
    if !succ {
        os.yield()
    }
    
问题出在最后的yield操作只是让出了CPU，但并不是将当前goroutine设置成阻塞状态。这个不同之处在于，前者只是丢到了READY队列的队尾，下一轮还是会运行，浪费CPU，而后者丢到了阻塞队列，除非满足了某些唤醒条件，它是不会再调度的。

在看云风的ltask的时候，发现它里面也要实现类似的模式，惊讶地发现它是[直接调用coroutine.yield](https://github.com/cloudwu/ltask/blob/master/consumer.lua)了，不禁想比较一下它为什么能这么做。发现ltask在调用select没有可用channel的时候，其实它就已经设置task的状态为阻塞了，跟Go不同，Go中没有暴露出将goroutine状态改为阻塞的方式。

还有一处发现是ltask中select的实现是逐个地对多个channel依次加锁，判断是否可用，释放锁。

ltask.select是支持操作多个channel，理论上跟Go应该是一样的。但这里跟Go实现方式不一样，引起了我的注意。

分析以后发现，ltask里面的写channel操作是不阻塞的，这样不会出现死锁的成环条件。不知道云风是碰巧设计的还是有意为之。总之若没有这项约束，这种实现就是隐藏较深的BUG。

-----------

为什么Go语言中不这么做？记得我在[一次技术分享](https://github.com/gopherchina/meetup/blob/master/Guangzhou/20150912/%E6%B4%BB%E5%8A%A8%E6%80%BB%E7%BB%93.md)的时候提到过这个问题。我们看一下：

    select {
        case c1<-1:
        case c2<-2:
    }
    
    select {
        case <-c2:
        case <-c1:
    }
    
假设两个goroutine分别在运行上面的两个select，如果select的实现方式是依次对每一项channel加锁，判断是否可用，解锁，这样的流程会出现什么问题呢？我们假设goroutine1判断c1，发现不可写，同时goroutine2判断c2不可读。然后，goroutin1判断c2不可写，同时goroutine2判断c1不可读，于是----死锁了。两个goroutine都进入了阻塞，并且再也没有机会唤醒！

select其实是一个整体，里面的资源不能独立对待。要么全部成功，要么失败，否则可能死锁。

所以在Go中的实现是，对select里面的资源全部加锁之后，才执行后面的操作。为了解决加锁时的死锁，Go是对channel的结构体按地址排序后，按顺序加锁的。select操作是一个比较有开销的实现。

我去网上查相关的资料，还看到有人问到select的实现，Dmitry Vyukov(Go调度器的实现者，大牛一位)回答，本来是可以把channel做成无锁的，但正是select的存在，让无锁的channel很难实现了。然后Rob Pike跟贴，说当年他还专门发过paper讲select的实现。

------------

再推广一下，其实这并不仅仅是在Go语言中的一个模式，其实在很多其它地方都有涉及。

宽泛地说就是执行多个资源的select，如果满足其中一个，就能继续执行。如果一个都不满足，则将当前的执行单元阻塞。将来在这些资源之一就成可用时唤醒执行单元。

我猜你大概能想到的，操作系统的select/epoll调用，或者fork以后多个进行的accept操作，都具有类似的模式。

那么还不难想到另一个：惊群问题。也就是，被唤醒的执行单元做了一次无意义的唤醒，然后再做一轮抢资源，只有一个胜出。
