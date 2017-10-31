## 系统执行一条SQL，到底慢在哪里？

如何跟踪一个分布式事务的所有行为？使用 [opentracing](http://opentracing.io/)！ opentracing 是一个标准的分布式 tracing 的协议。支持 Go, JavaScript, Java, Python, Ruby, PHP, Objective-C, C++, C# 等等许多的语言（暂时没看到 rust 的库）。

[一些基本的概念](https://github.com/opentracing/specification/blob/master/specification.md#the-opentracing-data-model)：一个 Trace 是由许多 Span 构成的一个有向无环图。Span 之间的边的关系叫做 Reference。

Span 里面的信息包括：操作的名字，开始时间和结束时间，可以附带多个 key:value 构成的 Tags(key必须是string，value可以是 string, bool 或者数字)，还可以附带 Logs 信息(不一定所有的实现都支持)，也是 key:value形式。

下面例子是一个 Trace，里面有8个 Span：

![](static/trace-span.png)

Reference 是记录 Span 之间的相互关系，目前定义了两种： ChildOf 和 FollowsFrom。ChildOf 很好理解，就是父亲 Span 依赖另一个孩子 Span。比如函数调用，被调者是调用者的孩子，比如说 RPC 调用，服务端那边的Span，就是 ChildOf 客户端的。很多并发的调用，然后将结果聚合起来的操作，就构成了 ChildOf 关系。如果父亲 Span 并不依赖于孩子 Span 的返回结果，这时可以说它他构成 FollowsFrom 关系。

可视化之后，将得到这样的视图：

![](static/trace-relationship.png)

## 举一个栗子

这里有一个很好的例子来说明，如何一步一步的支持 opentracing。

首先，我们需要知道整体架构大概的样子，比如：

![](http://opentracing.io/documentation/images/OTHT_2.png)

这是一个 Web 请求的例子，客户端发起请求，经过认证服务，计费服务，然后请求资源，最后返回。

接着，如果我们把 gRPC 和整个大致的流程串起来，可以看到一个大体的工作流程：

![](http://opentracing.io/documentation/images/OTHT_3.png)

如果我们继续细化组件内部的埋点，可能会变成这样子：

![](http://opentracing.io/documentation/images/OTHT_4.png)

又或者是这样子：

![](http://opentracing.io/documentation/images/OTHT_5.png)

具体的将以Go语言为例来说明。调研过 Go 语言的几个 opentracing 的实现，主要是

* [Appdash](https://github.com/sourcegraph/appdash/)
* [Jaeger](http://jaeger.readthedocs.io/en/latest/)
* [LightStep](http://lightstep.com/)

AppDash是sourcegraph开源的一个实现，上手最容易，它有一个in memory的后端可以直接嵌到应用里面，零外部依赖，但是后端和界面的功能做的比较弱。

LightStep是一个家做 tracing 的公司，后端以服务形式提供出来，各语言支持的库也挺丰富，但暂时没有具体测试过。

Jaeger是uber开源的实现，基于ZipKin的，功能比较强大，组件也比较复杂。还好有一个 all-in-one 的 docker 镜像。我们就用它了。

初始化好全局的 Tracer，就可以用了。

    tracingCfg := cfg.OpenTracing.ToTracingConfig()
    tracer, _, err := tracingCfg.New("TiDB")
    if err != nil {
        log.Fatal("cannot initialize Jaeger Tracer", err)
    }
    opentracing.SetGlobalTracer(tracer)

grpc 还是很方便的，有相应的包可以直接加上 tracing 功能：

	import (
        "github.com/grpc-ecosystem/go-grpc-middleware"
        "github.com/grpc-ecosystem/go-grpc-middleware/tracing/opentracing"
    )
    unaryInterceptor := grpc_middleware.ChainUnaryClient(
        grpc_prometheus.UnaryClientInterceptor,
        grpc_opentracing.UnaryClientInterceptor(),
    )
    streamInterceptor := grpc_middleware.ChainStreamClient(
        grpc_prometheus.StreamClientInterceptor,
        grpc_opentracing.StreamClientInterceptor(),
    )
    conn, err := grpc.Dial(
        addr,
        grpc.WithInsecure(),
        grpc.WithTimeout(dialTimeout),
        grpc.WithInitialWindowSize(grpcInitialWindowSize),
        grpc.WithInitialConnWindowSize(grpcInitialConnWindowSize),
        grpc.WithUnaryInterceptor(unaryInterceptor),
        grpc.WithStreamInterceptor(streamInterceptor))
        
## 哪些地方该埋点？

所有关键代码路径，执行比较耗时的地方(走io)，跟外部组件交互的地方(grpc)。

以 TiDB 为例，比如收到一个SQL请求执行的流程：

1. parse
2. compile
3. execute

事务两阶段提交的过程：

* prewrite
* commit

异步或同步取TS的过程；coprocessor执行过程；事务重试等等...

## 该如何埋点？

调用链信息必须通过某种方式一直传递下去，Go 语言是传递 ctx 参数，tracing 信息被藏在 ctx 里面。

    span := opentracing.SpanFromContext(ctx)
    ctx := opentracing.ContextWithSpan(ctx, span)
    
**尽量不要把 ctx 存储在对象里面！！！**

这个问题跟编程语言里面的静态作用域动态作用域有一点点像。[我以前也写过，context本质上是动态作用域](http://www.zenlife.tk/go-context.md)，动态作用域是很evil的。这次不扯无关的话题，只是强调下：

    对象创建时候的上下文，跟执行对象方法时候的上下文，并不是同一个
    我们应该始终trace使用时的上下文
    
举例一：request sender。假设有一个 sender 对象，并且这个对象可能是复用的。那么，创建对象对象的时候保存下来的上下文，跟调用 `sender.Send()` 的时候的上下文，有可能并不是同一个。

    sender := RegionRequestSender{ctx}
    sender.Send(msg1)
    // another message
    sender.Send(msg2)

举例二，ddl的问题。DDL对象在创建的时候有一个 ctx，它会起后台的 worker，前台收到消息会分发到后台worker。那么 tracing 的应该是：收到消息，分发到worker，worker处理完消息，返回结果的过程。每次来新的消息，都是一个新的 ctx，而不是DDL对象创建的时候那个ctx。假设需要cancel，也是cancel掉某一次消息处理的上下文，而不把把DDL对象cancel掉。

给事务加 tracing，坏的做法在 Begin 的时候把 ctx 存到 txn 对象里面：

    txn := store.Begin(ctx)
    txn.Commit()

好的做法，在Commit的时候把参数带上去：

    txn.Commit(ctx)
    
**trace 应该以一个请求的生命期(调用链)为单位，而不是以事务为单位**

由于 SQL 的特殊性，一个query跟一个事务并不是一一对应的关系。如果把 trace 以一个事务为单位，两个地方处理起来比较麻烦。一个是用户用 `begin ... commit` 发多次请求，这种情况必须将 ctx 存储起来。另一个是 retry 的处理。

retry 重试了事务就没了？这样不对。在retry里面重新取了ts，重新生成新的txn对象...但是其实还是同一个事务，这个信息非常重要，我们要记录下来。如果基于事务记录，这一块逻辑很乱，而基于一个请求的生命期，这个就比较容易。

但是我们还是要把事务和 trace 绑定起来。使用 Tags！为每个事务记录 txn.id 的 Tag 信息，就可以处理 `begin ... commit` 了。

    type Span interface {
        SetTag(key string, value interface{}) Span
    }
