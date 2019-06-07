研究了一下云风大神的skynet

大神的技术博客写得跟他名字一样飘逸，说实话单看博客我完全不知所云。

所以我只好看代码了。其实看了代码再回头看客博还是看得懂的。

果然代码才是程序员的通用语言啊

## skynet是什么？

按我的理解应该算是一个底层的消息通信框架。

其实就是一个RPC的系统。"所谓 RPC 调用，就是一个远程对象加一个方法名，加上若干参数。通过序列化方法，打包成一个数据包，查询到远程对象所在的服务地址，发送过去即可。"

## skynet用来做什么？

我猜测他应该是做游戏的gate服务器。认证的过程就可以看作一次RPC

举例来说，如果 Client 发起登陆验证请求，那么由给这个 Client 服务的 Agent 首先获知 Client 的需求。然后它把这个请求经过加工，发送到认证服务器等待回应（代码上看起来就是一次函数调用），一直等到认证服务器响应发回结果，才继续跑下面的逻辑。所以处理 Client 登陆请求这单条处理流程上，所有的一切都仅限于串行工作。当然，Agent 同时还可以相应 Client 别的一些请求。

## 实现的一些细节

首先我们接到消息后肯定无法立马处理完，为了不影响后面的响应，所以要搞个消息队列。 这个就是源代码中的skynet_mq那个东西。

整个框架是开了几条线程，相当于有个线程池，然后不断地从消息队列中取消息，分发消息。 这个在源代码中的skynet_start代码中，可以看到_start函数就是创建线程，线程执行的函数正是skynet_context_message_dispatch

我们知道 RPC 这东西的实现，就是先弄一个 master 出来，然后后面的 worker 向 master 注册说明自己提供了哪个可供调用的函数。

所以master要记录这些注册信息，这个是在 `skynet_handle` 文件实现的一个哈希表存储的。实现了一个句柄 handle 到 `skynet_context` 的映射。具体的注册这种是在 `skynet_harbor` 中实现的。

这里并不是只有一个 master 其它全是 worker 这种模式，而是每个东西都即是 master 又是 worker，即每个节点知道其它所有节点，这个东西就是 `skynet_context` 了，相当于进行相互通信的实例。

然后就是模块化的思想。每个服务提供者都做成一个模块，模块管理的代码就是 `skynet_module` 了。可以通过名字查到模块，这个是做成 .so 文件加载的。每个模块都实现了 create init release几个函数。

这些就把skynet的主体框架搭建好了：

    skynet_mq.c 消息队列相关
    skynet_handle.c handle到context的映射
    skynet_module.c 模块管理
    skynet_harbor.c 向远程节点注册自己
    skynet_server.c skynetcontext的实现，每个context是一个服务模块

然后我们看下主体流程

`skynet_start` 函数，先是 harbor,handle,mq,module 这些组件进行初始化

然后启用各个服务模块。如果是 standalone 则要启动 master 的 skynetcontext 然后先是 harbor 的 skynetcontext 接着 logger,snlua 等服务模块的启动

模块启动完毕之后开始线程池，进行消息dispatch的循环

----2012.9.4 修改-------------

云风最新的blog又出了一篇文章，这次总算把skynet讲清楚了． 我之前的理解还有是些地方有偏差的 http://blog.codingnow.com/2012/09/the_design_of_skynet.html#more
