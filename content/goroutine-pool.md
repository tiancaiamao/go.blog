Go语言的goroutine初始栈大小只有2K，如果运行过程中调用链比较长，超过的这个大小的时候，栈会自动地扩张。这个时候会调用到一个函数`runtime.morestack`。开一个goroutine本身开销非常小，但是调用morestack进行扩栈的开销是比较大的。想想，如果函数的栈扩张了，有人引用原栈上的对象怎么办？所以morestack的时候，里面的对象都是需要调整位置的，指针都要重定位到新的栈。栈越大，涉及到需要调整的对象越多，调用morestack的时候开销也越大。

我们可以写一个简单的bench，这个函数递归调用是比较消耗栈空间的：

    func f(n int) {
      var useStack [100]byte
      if n == 0 {
        return
      }
      _ = useStack[3]
      f(n - 1)
    }

下面是对比测试：

    func bench1() {
      var wg sync.WaitGroup

      for i := 0; i < benchCount; i++ {
        wg.Add(1)
        go func() {
          for j := 0; j < 15; j++ {
            f(2)
          }
          wg.Done()
        }()
        wg.Wait()
      }
    }
    func bench2() {
      var wg sync.WaitGroup

      for i := 0; i < benchCount; i++ {
        wg.Add(1)
        go func() {
          f(30)
          wg.Done()
        }()
        wg.Wait()
      }
    }

两者工作量是一样的，但bench1不会触发`runtime.morestack`，bench2会触发，可以看到结果相差了一个数量级：

    bench1 used: 52480486 ns
    bench2 used: 559074503 ns

我们的项目里有发现这个问题，morestack的CPU时间几乎占到了10%。隔壁友商[cockroach也发现这个问题](https://github.com/golang/go/issues/18138)。怎么解决呢？

有两个方向，一个是初始分配更大的初始栈，比如起goroutine后，先调用下面这个函数，将栈扩到8K：

    // reserveStack reserves 8KB memory on the stack to avoid runtime.morestack.
    func reserveStack(dummy bool) {
      var buf [8 << 10]byte
      // avoid compiler optimize the buf out.
      if dummy {
        for i := range buf {
          buf[i] = byte(i)
        }
      }
    }

提前预留栈空间的方式，相比程序跑到后面栈不够了扩栈，开销低一些。cockroach是这个做法。实测时我发现morestak的开销并没有被消除，而是转移了。

所以我要说的是另一个方向，goroutine pool。

其实goroutine这么轻量的东西，其实本身做池意义并不大，随用随开，用完就扔，挺好的。然而在触发morestack的情况下，这个开销就有点高，在火焰图上是可以抓到的(go pprof不那么敏感)。采用pool之后，如果goroutine被扩栈了，再还到pool里面，下次拿出来时是一个已扩栈过的goroutine，因此可以避免morestack。

接下来说说这个goroutine pool该怎么写。

我希望接口是这个子的：

    pool = New()  // 创建pool
    pool.Go(func() {
        // do something
    })

跟调用

    go func() {
    }()

效果是一模一样的，只不过`pool.Go`执行闭包函数以后，goroutine不是退出，而是放回到池子以，供下次再调用。

为此，我们要将goroutine抽象成一种资源，

    func (pool *Pool) Go(f func()) {
         res := pool.get()
         res.run(f)
         // pool.put(res) 这里还不能归还，后面讲为什么
    }

这个资源比较特殊，它是由一个channel和一个后台goroutine组成：

    type res struct {
         ch chan
         pool *Pool
    }
    go func(r *res) {
       for work := r.ch {
           work()
           r.pool.put(res)
       }
    }

run只需要往channel里投喂，后台的goroutine拿到work后就会执行它。

    func (r *res) run(f) {
        r.ch <- f 
    }

这里有个细节，执行完以后归还到goroutine pool，要留下work执行完以后做，因为`res.run(f)`是非阻塞的。

池子的实现是比较容易的，用链表就可以了，把res串起来，首尾结点，尾进头出。不过要注意线程安全，想优化可以把首尾结点的访问分开加锁，甚至用无锁队形来实现。

注意到，我没有为pool设计Close接口，为什么？这是有意为之的。那么，池子里的goroutine什么时候释放呢？设计成过一段时间不用自动回收。

从经验上看，只要涉及重用goroutine的代码，都有很大概率发生泄漏问题，尤其是调用close以及close的实现。分配出去的资源，它是属于池子呢，还是不属于池子呢？关闭的池子时候是等资源还回来呢？还是不等呢？如果归还资源的时候，池子已经关闭了呢？关闭的瞬间，跟正在读写池子的请求，如何处理好加锁呢？所以这是一个设计上的问题。

每次使用都打上最后使用的时间，多起一个回收的goroutine定期的扫描池子内的goroutine，如果很久没用过，就回收掉。这个回收的goroutine如果发现池子里一个goroutine都没了，那它自己也退出，非常干净，完全不会有泄漏。退出前要加个标记，如果池子又被使用了，重新创建回收goroutine起来工作。

完整的代码，可以看看[这个PR](https://github.com/pingcap/tidb/pull/3752/files)里面。
