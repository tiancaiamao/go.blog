之前有思考过关于 cora 的多核支持，记录一下备忘。先上结论，就是当前设计由于 GC 暂时不打算支持多线程，这个限制非常强，导致最终多核支持只能每个核像独立的进程那样工作，只能提供类似 rpc 那种级别的通讯机制，让不同核之间协作。

## CML

[CML](cml-vs-go.md)之前已经实现了，不过只实现了单线程级别，有 coroutine 来并行。并行不是并发，当我思考多核支持的时候，第一个想到的就是如何继续把 CML 扩展到多核去。

理论上 CML 是非常灵活的，它可以让 coroutine 并发，让 thread 并发，甚至让 coroutine 和 thread 之间混用的并发。在这个模型下面，只要实现 try 和 block，就可以成为一个 operation，然后 perform operation 就可以了。perform 就是先 try 一下，如果不 block 则继续往下执行，而**如果会 block，则当前的执行单元挂起，放到阻塞队列中**，特定情况下唤醒后从阻塞队列放到就绪队列。对于 coroutine，由语言的机制来提供上下文的保存和切换，而对于 thread 这类，可以用 futex。对于 thread 级别的计算单元，挂起后并不是将当前执行的东西切换出去，然后切换另一个函数来执行，而是 thread 卡在那里等待恢复，因此 futex 就够用了。

不考虑 coroutine 和 thread 混合调用这种模式，就看看正常的 thread 和 coroutine 两层划分，CML 的并发模型从单线程到多线程需要做哪些改动。
有多少核就起多少 thread，然后是调度器那边的改动。就绪队列需要做成并发安全的队列，简单起就只保留一个就绪队列，全局竞争。然后 block 在 channel 上，或者是 block 在网络或 io 上，会涉及阻塞队列，这里相关的操作也得做成线程安全的。

CML 是不带缓存的 channel，所谓的 channel 只是 coroutine 的交汇点，去进行值的交换。单线程的时候，值在不同的 coroutine 之间流动，不会有啥问题。而多线程的时候，如果垃圾回收是每个线程绑定各自的垃圾回收器，独立运行，不同线程的分配的对象是无法跨到另个的线程去使用的，这里 channel 交换值的时候就有问题了。

就绪队列就是 coroutine 的队列，全局一个，所有的 thread 去抢这个队列上面的 coroutine 执行，这是一个简单的非 M:N 的调度，但是 coroutine 在不同的 thread 上面移动，也是当前的 GC 是无法支持的。

## Go

Go 语言其实在上古版本里面，是没有 M P G 的概念的，也是只有两层，thread 去全局的就绪队列中抢 goroutine 来执行。到后来才引用了 M P G 的概念，减少竞争和提升资源的利用率。

如果对应到 cora 里面，M 还是线程，P 就是 cora 的 VM 的概念，然后 G 就是 cora 中的 coroutine。不过 M 和 P 会是一比一的，在 Go 语言里面 M 是 Thread，可以大于 P 的数量，当 Thread 进入到 OS 中阻塞时，可以起新的 M，而保持 P 跟核数一致。如果在 cora 中实现成每个 VM 调度各自的 coroutine，是可以的，关键点在于抢占式调度无法实现。挂在 M 上的 G 可能有些忙，有些闲，这就需要抢占调度去从忙的 M 上窃取一些 G 过来执行。而 cora 里面是没办法跨 thread 去移动 vm 或者移动 coroutine 的。

## skynet

最后想到的可能参考的是云风的 skynet，所以我又去读了一遍它的设计。如果说 Go 那边是让 goroutine 成为基本的执行单元，skynet 这边则是强调以消息为中心。如果说思考 Go 那边的演进是，有了 goroutine 概念之后，从线程取 goroutine 就绪队列，到 M P G 的调度器优化，skynet 这边则更适合从 epoll 回调的角度来思考和推导。

假设我们没有 coroutine 这样的基础设施的时候，如果我们用 epoll 的方式来做并发编程，那么它就是一个以回调为中心的。收到某个 epoll 消息后，回调某个函数。假设我们多线程之后，我们把回调变成消息处理，将直接在某个 thread 上面回调某个函数，变成将一个消息，传到一个全局队列中。后面由 thread 去队列抢这条消息，抢到之后执行相应的消息处理函数。这就是 skynet 的基本原型了。

它的思维模式是以消息处理为中心的，泛化之后，消息的来源并不是只有 epoll 这一个情况，任务一个服务都可以产生消息，然后把消息发送到一条消息总线上去；注册不同的服务，处理不同的消息类型；thread 数量跟核数一样，然后 thread 去消息总线上抢消息，抢到消息之后再看是对应的处理这条消息的服务，去调用消息处理函数。服务对消息的处理需要是无状态的，这样无论哪一个 thread，拿到消息后都可以调用对应的服务处理回调函数。

当我们把消息，和消息的处理函数，整合到一起看的时候，它就是一个闭包，就一段一段的回调函数加闭包状态，就是一个可以执行但是没有分配特定计算资源的东西。当它拿到了计算资源 -- 也就是 thread 的时候，就可以执行了。lua 对于 skynet 的意义其实没那么独特，skynet 的服务可以用纯 C 写，也可以用 lua 写，理论上它可以用任意一个脚本语言来写，只要这个脚本语言提供了 VM 的概念，然后可以把 VM 暂停，以及让 VM 继续执行。这个 VM 的作用，是把一个一个回调，串起来让它像是串行化思维的。

来举个例子，如果我们要从网上下载一个东西，然后写到文件里面，然后继续执行一些什么，那么这个逻辑就是:

- vm 发送网络 io，然后把自己挂起来
- vm 得到下载文件的结果，执行下一步写文件，写文件 io，又把自己挂起来
- vm 得到写文件 io 的结果，继续向下执行

从上层的角度看它是不阻塞的，从实现的角度其实是 vm 涉及往不同的服务发消息，以及收到消息后再继续处理消息的过程。这一层 vm 的作用是保留到了 vm 里面，而不是用闭包+状态机来保留状态。

---------------

guile scheme 那边就实现了 CML 模型，为什么 cora 不能用 CML 的方式实现多核并发。因为 guile scheme 它的垃圾回收是全局的，即全有对象都是从全局的垃圾回收器分配出去的。然后在垃圾回收算法那边需要支持并行。而 cora 的垃圾回收是每线程一个的。

skynet 那边，用的 lua 语言为什么可以，而 cora 不能用这种方式？lua 也是单线程 GC 的。不同之处在于 lua 的 GC 跟线程是没有绑定关系的，它是精确的 GC。而 cora 这边 GC 是绑定了线程的，保守的垃圾回收。由于 cora 的 GC 跟线程绑定之后，cora 的 VM 就不能在线程之间移动了。而 lua 那边 vm 是可以在这个线程上创建，挂起，而后在其它线程上面恢复的。

那为什么当前的 cora 的 GC 要设置成这个样子呢？既不能全局一个垃圾回收器，又不能去掉跟 thread 的绑定关系。不支持全局一个是因为并行的实现复杂度，当前已经做了增量和分代，调试的过程我知道这个正确性是非常难搞的，加入并行之后我一定 hold 不住。而没有做成精确的 GC 是因为保守的 GC 在 API 上面是更加用户友好。

那么剩下的多核支持，就只能像多进程模型那种了。可以做的是，每个线程上面运行一个 cora，各自独立的调度器/GC/堆，而相互之间只能像网络 socket 那样通讯，倒是可以提供一些 rpc 的基础设施，毕竟是在同一个进程内的，可以不用走到内核再走回来那么重。
