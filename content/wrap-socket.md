五一宅在家里写代码，估计删删改改的有个[1000多行](https://github.com/tiancaiamao/grt/tree/0.0.1/async)吧。云风常常一个周末敲个大几千行的东西出来，我还是比较相信的。对于撸自己比较熟习的东西，一遍一遍，每次都期望比上次做的更好一些。基本上一个东西，要写上好多次，代码和设计才会变得优雅。

自己也是那种不写代码手会痒的类型，每隔一段时间，狂暴的血液就会沸腾一阵。那种写到半夜2、3点，越写越兴奋停不下来的感觉。大概真正的程序员都经历过这感觉。这次是封装socket玩。

估计除了新手第一次看socket，没有人平时写代码是socket/bind/listen不做封装拿着用的。更多的是使用现成和库或框架。其实封装socket是个挺好玩的事情。

我希望封装后应该是方便使用的API，喜欢的类型是那种，来一个请求，开一个thread那种同步的API。我希望它是上层阻塞底层不阻塞的，希望thread是轻量的。最后，它的代码看起来大概应该是[这个样子](https://github.com/tiancaiamao/grt/blob/0.0.1/async/demo2.lua)。

我说的封装，就是一步一步的弄到这个demo能跑起来(废话)。过程是一层一层的。

第一步是封装C给lua用。封装基本的socket，以及epoll。我封装得比较浅，基本就没做太多，只是把C导出到lua了。关于epoll封装，云风写过一遍[回调还是消息队列](http://blog.codingnow.com/2013/07/callback_or_message_queue.html)，我是蛮认同的。C让回调lua是一个不太干净的东西，lua应该是在C之上更高层次。

这一层还涉及到一个跨平台的事情，尽量把平台相关的在比较低层次的API中封装了，高层就可以统一。开始自己写完epoll的，发现kqueue还是不太熟，又要偷懒，后来干脆直接“偷”了[云风封装好的头文件](https://github.com/tiancaiamao/grt/blob/0.0.1/async/socket_poll.h)来用。

封装到这里的时候可以在lua里面交互式的跑一跑，这种交互式的跑tcp的感觉还是挺好玩的，中间可以把各步骤断点下来，看tcp状态变迁之类的。

第二步是封装异步API。还是按中规中矩的写法来的，异步API代码大概是[这样子的](https://github.com/tiancaiamao/grt/blob/0.0.1/async/demo1.lua)。其实封装成这样子也是能够用了，就是我不喜欢写异步，不喜欢事件循环，每次看到异步回调就感觉很反人类。

但是这一步是需要做的，之前[写过博客](lua-practice.md)就说过，异步是同步的基础，lua提供了协程机制，在异步之上封装同步不难。

写异步API封装这一块代码的时候，要多注意下细节处理，这是比较考验基本功的。epoll的水平边沿触发；各种error区分；读到长度为0关连接；主动关连接时刷缓冲保证数据可靠传输。呵呵，我这是demo代码乱写的，别当真。

write是异步写的，也就是先保存在应用缓冲，到EPOLLOUT事件的时候刷过去。还有记得刷完后关掉EPOLLOUT，不然会一直提醒的。说到这里，lua如何用两个table来模拟队列，而且还要支持推回队列头功能，可以去看[代码](https://github.com/tiancaiamao/grt/blob/0.0.1/async/async.lua#L71)。推回队头需求是因为write可能到缓冲写满，这里我们要把剩下的数据先推回去，下次再发。

第三步了，封装成同步API。

有lua的协程支持，异步封装同步很容易的。就这几行：

	local Conn = AsyncConn:new() --继承异步的类
	function Conn:read()
		self.co = coroutine.running() --保存当前coroutine供将来使用
		return coroutine.yield() --yield掉会阻塞的操作
	end
	function Conn:on_read(data) --重载on_read回调
		coroutine.resume(self.co) --在回调函数中恢复之前保存的coroutine
	end

Thread还是要的。直接协程不太方便，可以封装一下。
lua的协程是非对称协程，就是不能够A B之间相互的resume，只好A yield加调度器，由调度器决定resume到B，B也是先yield回调度器而不是直接到A。

还有遇到的一个问题是，我开始把eventloop作为一个普通的Thread跑，但是新建的Thread跟eventloop的Thread谁先调谁后调，时序问题容易出bug。我在eventloop里面是不做yield了的，这样异步API干净一点不会跟Thread有耦合。于是有一种情况是：新建出来的Thread并没有立刻执行而不放到队列尾，执行到eventloop的线程，系统便不再切换了，新线程永远得不到机会运行。
所以后来我把eventloop单独抽到完全没有用户线程可执行了才调用。

还有一个很扯的，connection加到事件循环的时机问题。要等线程建出来之后，否则线程还没出来，有事件来了`Conn:on_read`就回调，这时里面的`self.co`还是空指针。

最后。。。这是个概念验证级别的的demo。确信自己能弄出喜欢的socket风格来。

目前这个demo中是用的cotoutine来模拟线程的。
完整的将是grt这个项目(代码所属的父级目录)，几年前挖的坑，至今还没填的。其实就是Go的那一套runtime，用C+lua完全可以弄出来。线程和lua虚拟机M:N的调度来充分压榨CPU，channel通信，基于协程的轻量并发，友好的同步API(上层阻塞而底层不阻塞)。
