# hive，skynet以及go语言

这里的hive和skynet都是云风大神的开源项目。skynet是一个基于actor模型的开源并发框架。hive是skynet简化并去掉了一些“历史包袱”之后重新设计的框架。go是google开源的一门编程语言。

为什么把这些东西放到一块呢？因为我看了一下它们的代码，发现很多地方有惊人的相似之处，这些正是大牛们长时间积累沉淀下来的东西，非常有价值，所以这篇文章将它们拿到一起分析一下。

# skynet到hive的演化

以前也写过一些关于[[http://zenlife.tk/actor.html][actor模型]]和[[][skynet]]的文章，最新的skynet代码我没有跟进。按云风的说法，skynet已在项目中使用，不太好做大的修改，所以重新写了hive项目。

1. 更精简的核心部分

hive相比skynet更加精简，比如说skynet做了rpc方面的东西的，而hive中去掉了。原因是像这种东西放在核心层做，使得核心复杂化，并且不一定能满足应用层的各种场景。不如移到上层去实现，保持精简的核心层。

2. hive直接绑定lua

既然像skynet实现消息传递的核心代码后，后面一定会绑定一个动态语言做上层，与其单独做一个lua的服务模块，不如直接就绑定lua。我觉得这个还是带来了不少好处的。可以看一下代码上的区别，这是以前skynet中对actor的定义：

	struct skynet_context {
		void * instance;
		struct skynet_module * mod;
		uint32_t handle;
		int ref;
		char result[32];
		void * cb_ud;
		skynet_cb cb;
		int session_id;
		uint32_t forward;
		struct message_queue *queue;
		bool init;
		bool endless;

		CHECKCALLING_DECL
	};

其中的核心就是skynet_cb和message_queue，一个是回调函数， 个是消息队列。而在hive中变了，可以说skynet还是偏一点C的，而hive是lua的。它的actor是这么定义的：

	struct cell {
		int lock;
		int ref;
		lua_State *L;
		struct message_queue mq;
		bool quit;
		bool close;
	};

它的actor就是一个lua_State，一个message_queue。

一个skynet_context就是一个服务，在skynet中每个服务要写处理特定消息的回调函数。

	struct skynet_message {
		uint32_t source;
		int session;
		void * data;
		size_t sz;
	};

这是消息的定义，然后是消息处理的回调函数：

	typedef int (*skynet_cb)(struct skynet_context * context, void *ud, int type, int session, uint32_t source , const void * msg, size_t sz);

而有了lua则可以直接在消息中传lua函数了！这是一个质的飞越，灵活性大大提高。另一个actor中有一个lua运行环境，你要给它发什么消息，直接可以发一个lua函数过去让它执行好了。这个提升相当于从双方约定允许发送和处理什么消息，到rpc屏蔽底层细节的飞越。

3. 网络消息的处理 

skynet中比较区分外部消息和内部消息，外部消息比如来自于网络的。它专门用了一个gate服务，所有网络消息通过gate进来，然后分发给各个actor。gate那边管理好所有连接，并且可以做ringbuffer的一些内存管理优化。但后面云风发现与外部进行交互是不可避免的，于是出现了各个服务可能不走gate直接处理网络服务，系统中也存在多个epoll的端口了，这样对效率有影响。

于是，在hive中直接封装了epoll非阻塞操作，提供给上层同步的接口。

还有许多其它的细节我也没认真地看。

# hive和go的比较

hive和go的代码我都看过，发现有些想法惊人地相似。为了充分地利于多核的并发优势，它们都选择了协程，go中是goroutine，hive框架中是借助lua的coroutine，非常轻量。协程之间不会有加锁之类方式的处理数据依赖，不会通过共享内存来通信，而是通过通信来共享内存。go中是channel，hive中每个actor都附带一个消息队列。如果遇到协和执行不下去了，则会暂时地将它yield，直接条件满足时继续。go中是通过分段栈实现保存一个goroutine的低开销，而hive更省，直接利用lua虚拟机。

在底层实现上，它们都是开了几条物理线程，不停地取一个协程执行，如果要yield就将协和放到队列中等待时机重新拿出来执行。调度方面go要做得完善一些，毕竟hive代码量小。

不过在保存上下文上，hive更牛一些。据云风说保存一个coroutine只要200到300B，每个lua_State不到10K，而go的每个goroutine则至少需要4k以上，即使使用分段栈技术，所以还是没有lua轻量。只要是按栈去实现的保存上下文都不可能更轻量的，没办法。而且分段栈带来的很大一个负作用就是与C的兼容性，其实cgo并不那么好用的。lua使用虚拟机的，与C的兼容性堪称完美。不过也不是完全没有代价，C的数据与lua栈数据的传递也是一笔额外的开销。

在网络处理方面，从skynet改版后的hive与go的做法是相同的，底层是epoll/kqueue机制的异步io，上层提供给用户的阻塞的io接口。我觉得这才是人性化的方案，异步加回调那种绝对是反人类的。

底层实现上也是相同，调用上层的网络api后导致阻塞，则会把当前的协程yield掉。有一个后台线程不停地做poll，如果收到数据则会唤醒相应端口的协程。

不同的地方是通道方面。Go提供了first class的channel，这个通用性更强一些。而hive则受了更多erlang的影响，每个actor绑定一条消息队列。

虽然我是Go语言粉丝，毕竟不是低端脑残粉。看了hive的代码后，有时候我甚至觉得hive做得更好一些。完美兼容C是个很大的优势，比如说内存管理可以自己选择让lua垃圾回收或者自己手动管理。甚至，使用完之后，直接释放一个lua_State都不用进行垃圾回收。虽然Go也可以自己申请一块大内存后手动管理，但总不是像直接使用C那么爽。随便进行比较一下，代码量短小易控方面，hive胜。coroutine的开销上，hive胜。DSL设计上，hive胜。内存管理的灵活性方面，hive胜。

# 关于网游
最近想找点有意思的东西做，就想到了网游。找个适合研究的网游并不容易。无非就是直接开源的网游，开源的服务器框架，或者就是源代码泄露了的私服。

先下了个diamonon，这是个开源的网游。看了下2d界面，丑暴了。3d的没编译出来。随便看了一下它的源代码，感觉比较挫，还停留在很早期的时代。IO复用还用的是select做的，而不是poll。服务器方便也没做任何区分，整个编译出来一个服务器文件，包括了loginserver，gameserver，database。准确点说database也是没有的，数据直接持久化到本机的文件系统。

然后是planeshift，这个也是个开源网游，3d的。编译到一半放弃了。想了想有名的T端M端之类的，C++什么的最无爱了，况且太大的代码量就没有去研究的欲望了。

接着就是考虑源代码泄露的一些游戏，传奇什么的代码应该不难搞到，但游戏没玩过，研究起来就没什么意思了。其实找个自己玩过的比较熟的还是蛮不错的。最终回到了darkeden。这个游戏我最熟习，玩了很久很久。10年的时候也读过一点点服务端的代码，虽然版本比较旧。

要是能找到一份服务端代码以及配套的客户端，还是可以玩玩的。初步计划，loginserver可以用我熟习的Go语言写，gameserver用hive。数据库能避开就先尽量避开。前期先求一个能跑起来的东西，主要是把packet搞定。写了这么多，有点偏题了。目前只是计划，具体的实施得留到下一篇文章了...
