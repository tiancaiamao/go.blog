Go 语言的 context 包里面有个 Context 接口。

    type Context interface {
        Deadline() (deadline time.Time, ok bool)
        Done() <-chan struct{}
        Err() error
        Value(key interface{}) interface{}
    }
    
同时还提供了一个 WithCancel 方法：

    func WithCancel(parent Context) (ctx Context, cancel CancelFunc)
    
WithCancel 接受一个 Context 并返回一个 Context，返回的 ctx 里面的 Done() 函数会返回一个 chan，当 cancel 调用时，这个 chan 会被关闭。还有一个特性，**当 parent 的 Done() 关闭的时候，孩子 ctx 的 Done() 也会被关闭**。

这个特性很重要，实际使用中，WithCancel 让调用者之间构成了一颗树型结构，调用者一般是在不同 goroutine。如果调用父亲 context 的 cancel，就可以将孩子的 Done() 的 chan 同时关掉。因为 Go 语言里面的 goroutine 是协程，这个机制成了一个标准的控制 goroutine 之间作业交互的手段，在孩子 goroutine 里面一般会监听父亲的 cancel 信号：

    select {
        case <-ctx.Done():
            return
    }
    
那么我说坑爹的 context.WithCancel 是为什么呢？ 我们来看一下它是怎么实现的。

关键点是在于，怎么样实现父亲的 Done() 关闭时，孩子的 Done() 也被关闭。

一种方式是，父亲和孩子使用同一个 channel。这样子关闭父亲跟关闭孩子就是同一个 channel。那这个做法有什么问题呢？父母和孩子都关闭同一个 channel，它会不会调用多次？同一个 channel 关闭多次就 panic 了，当然这个可以绕过去。另外一个问题，假设父亲和孩子都是在同一个 channel，那么许多 goroutine 都将作用在这同一个 channel 上面，实现里面就是一个很粗粒度的锁了。前面我也提到过[使用 context 时的问题](go-context.md)。Go 是不是这么实现的呢？ 不是。WithCancel 的说明里面写得很清楚了： WithCancel returns a copy of parent with a new Done channel.

另一种方式，一个很挫的方式，专门起一个 goroutine 来监听，如果发现父亲关掉了，那么就关掉孩子：

    var parent, child chan struct{}
    go func() {
        select {
            case <-parent:
                close(child)
        }
    }

感觉这样实现太重了？标准实现到底是不是这样子的呢。

      func WithCancel(parent Context) (ctx Context, cancel CancelFunc) {
          c := newCancelCtx(parent)
          propagateCancel(parent, &c)
          return &c, func() { c.cancel(true, Canceled) }
      }
    
再看看这个关键的 propagateCancel 函数：

    func propagateCancel(parent Context, child canceler) {
        if parent.Done() == nil {
            return // 优化：父亲 Done 为空，所以不需要处理父亲关闭时，孩子关闭
        }
        if p, ok := parentCancelCtx(parent); ok {
            ...
        } else {
            // 亮瞎我的狗眼了，真的是开 goroutine 实现的。
            go func() {
                select {
                case <-parent.Done():
                    child.cancel(false, parent.Err())
                case <-child.Done():
                }
            }()
        }
    }
    
中间有一个判断 parentCancelCtx 函数走另一块逻辑是做什么的呢？它是一个特殊的优化的实现，父亲 context 里面维护一个 map，记录自己的孩子。添加孩子的时候就把孩子加到这个 map 里面。这样，在父亲 cancel 的时候，把这个 map 里面的孩子也 cancel 掉，就可以实现关闭父亲时自动关闭孩子了，不需要起 goroutine。

遗憾的是，这个优化只认识标准库里面的几个 context，也就是：

    func parentCancelCtx(parent Context) (*cancelCtx, bool) {
        for {
            switch c := parent.(type) {
            case *cancelCtx: // 标准库的 WithCancel 返回的 context
                return c, true
            case *timerCtx: // 标准库的 WithTimeout 返回的 context
                return &c.cancelCtx, true
            case *valueCtx: // 标准库的 WithValue 返回的 context
                parent = c.Context
            default:
                return nil, false
            }
        }
    }
    
我发现这个问题是在我们项目代码中，火焰图抓到 propagateCancel 函数的比例有点高。我在在代码里面弄了一个自定义的 Context 实现，它是这样子：

    type Backoffer struct {
        context.Context
        ...
    }

拿它当 context 使用时，每次 WithCancel 就会后台绑定起一个 goroutine。虽然 Go 语言的 goroutine 是很轻量的，但是 cheap but not free 呀！

结论就是，WithCancel 对标准库的几个 context 实现做了特殊优化，不会开启 goroutine，然而对用户实现的 context 非常不友好，会额外开启 goroutine，这太坑爹了。
