服务器启动过程
psServer的Initialize函数，先加载psserver.cfg配置文件，然后初始化数据库，配置管理，加载materials，加载mesh，加载map，初始化缓存管理等等...总之就是初始化psServer类中的各种管理类。netManager这边会新开一条线程。

主循环
在psServer的MainLoop中，会调用eventmanager的Run，这个是在主游戏线程中的，所有的消息和事件都会在这个线程中处理。事件是基于时序的，Run函数中会有个循环，不停地取事件，看在当前时间是否需要trigger，然后将事件进行Publish。

分发订阅
EventManager是继承自MsgHandler的，MsgHandler中有分发和订阅的函数。在Publish函数中，会获取订阅者的handler，然后调用它的处理函数：

```
handlers = subscribers.GetAll(mtype);
sub.subscriber->HandleMessage(message, client)
```



源文件：
server/psserver.cpp 服务器类
util/eventmanager.cpp 事件管理器
net/msghandler.cpp 消息的分发订阅