skynet是一个框架。库的意思是，你调用它。框架的意思就是说，它调用你。所以在不搞清楚整个流程之前，会有种被编程的感觉。最近想拿skynet做点东西，所以看一看skynet的流程。

## 读配置

	skynet example/config

参数是配置文件，配置文件中会有

* thread 线程数信息
* module_path 模块路径信息
* harbor harbor的id
* bootstrap 自举
* daemon 守护进程

## 系统初始化

	skynet_globalinit();
	skynet_env_init();

`skynet_globalinit`设置了线程本地存储。`skynet_env_init`创建了一个Lua虚拟机提供get和set。然后到了`skynet_start`函数。在`skynet_start`函数中：

	skynet_harbor_init(config->harbor);
	skynet_handle_init(config->harbor);
	skynet_mq_init();
	skynet_module_init(config->module_path);
	skynet_timer_init();
	skynet_socket_init();
	
每个skynet节点者有一个harbor与其它skynet节点通信。handle作用是从id或者服务名字得到`skynet_context``。mq就是初始化skynet的消息队列。

module是需要说一下的，一个符合skynet约定的so文件就是一个module，也称为一个服务。这个so文件中要包括create函数，init函数，release函数，分别要命名为模块名_create/模块名_init/模块名_release。`skynet_module_init`函数并没有初始化具体的模块，只是初始化了模块管理自身。后面系统会调用`skynet_module_instance_create`和`skynet_module_instance_init`来执行具体某个模块的创建和初始化。

timer和socket都是肯定要用到的非常基础的组件，在这里进行初始化了。

## 自举

	bootstrap(config->bootstrap);
	
在这个函数中启动了第一个skynet_context，默认情况下是执行snlua bootstrap。

在这里兜一圈，会最终运行到脚本bootstrap.lua中去。而bootstrap.lua最终会调用到config中指定的start脚本，在默认的例子中也就是example/main.lua。

如果想自己定义系统的启动行为，就通过配置config->bootstrap。

## 启动

	_start(config->thread);
	
在这个函数中会创建三条线程分别运行monitor，timer，socket。然后根据config中配置的线程数，创建线程并运行worker函数。

## 线程池+消息队列

然后就进入了传统的线程池+消息队列的循环中。每个worker会不停地取一条消息，查找这条消息是发给哪个`skynet_context`的，然后调用这个`skynet_context`相应的callback处理这条消息。
