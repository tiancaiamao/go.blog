## 介绍

把分布式系统中各个组件的工作汇总起来，就可以得到一个全面的跟踪系统。

每个公司都会有一套自己的分布式跟踪系统。Google的Dapper，Twitter的zipkin，淘宝的鹰眼，新浪的Watchman，京东的Hydra，唯品会的Microscope，窝窝网的Tracing。

重要的基础设施。

## 应用场景

一次竞价请求中要经历哪些事情？广告位查不到会怎么样？cookie mapping查不到会怎么样？调用栈。

竞价的平均QPS是多少？最高QPS是多少？波动情况如何？监控QPS。

为什么这个请求很慢？是哪一个环节出了问题？监控latency。

数据库的请求量突然上涨了，如何排查来源是怎么样的？链路分析。

这个操作需要依赖哪些东西？是数据库还是消息队列？如果某个redis挂了，哪些业务会受影响？依赖分析。

## 架构

(~~stolen from~~)Inspired by 鹰眼

![](./static/鹰眼架构.png)

处理过程包括应用内部埋点，日志数据收集，在线和离线的数据分析，结果的存储和展示。

## 埋点

不能造成性能负担：一个价值未被验证，却会影响性能的东西，是很难在公司推广的！

因为要写log，业务QPS越高，性能影响越重。通过采样和异步log解决。

	type Span struct {
		TraceID    int64
		Name       string
		ID         int64
		ParentID   int64
		Annotation []Annotation
		Debug      bool
	}

Span代表某个特定的方法调用，有一个名称和一个id。由一系列的标注组成。

	type Annotation struct {
		Timestamp int64
		Value     string
		Host      Endpoint
		Duration  int32
	}

Trace是关联到同一个请求的一系列的Span。

## 收集

每个机器上有一个deamon做日志收集。业务进程把自己的Trace发到daemon。

daemon把收集Trace往上一级发送。

多级的collector，类似pub/sub架构。可以负载均衡。

对聚合的数据进行实时分析和离线存储。

离线分析需要将同一条调用链的日志汇总在一起。

## 分析

调用链跟踪：把同一TraceID的Span收集起来，按时间排序就是timeline。把ParentID串起来就是调用栈。

抛异常或者超时，在日志里打印TraceID。利用TraceID查询调用链情况，定位问题。

依赖度量：

- 强依赖：调用失败会直接中断主流程
- 高度依赖：一次链路中调用某个依赖的几率高
- 频繁依赖：一次链路调用同一个依赖的次数多

![无耻地盗图](./static/链路分析.jpg)

离线分析按TraceID汇总，通过Span的ID和ParentID还原调用关系，分析链路形态。

实时分析对单条日志直接分析，不做汇总，重组。得到当前QPS，延迟。

## 存储

数据保留两个星期。

## 展示

必须能读才有价值。

# 技术选型

zipkin算是整套的解决方案，但是按照它的get start，装不上！

打算自己组装轮子。尽量采用Go语言的！

埋点肯定是自己做的。可以参考[这个](https://github.com/spacemonkeygo/monitor/trace)，但是性能方面要注意下。

日志收集系统听说有flume/scribe等。知乎开源的kid看了一下，很小巧，redis的pub/sub协议很不错。heka的可扩展性比较好，实时分析应该可以直接做在里面。

展现如果有前端帮忙可以考虑ECharts或D3.js，不懂前端。graphite可以做数据展现。在osx下安装，依赖好麻烦!

初步决定：Heka + Influxdb + Grafana

# 展望

tracing和monitor的区别。

monitor可分为系统监控和应用监控。系统监控比如CPU，内存，网络，磁盘等等整体的系统负载的数据，细化可具体到各进程的相关数据。这一类信息是直接可以从系统中得到的。应用监控需要应用提供支持，暴露了相应的数据。比如应用内部请求的QPS，请求处理的延时，请求处理的error数，消息队列的队列长度，崩溃情况，进程垃圾回收信息等等。monitor主要目标是发现异常，及时报警。

tracing的基础和核心都是调用链。相关的metric大多都是围绕调用链分析得到的。tracing主要目标是系统分析。提前找到问题比出现问题后再去解决更好。

tracing和应用级的monitor技术栈上有很多共同点。都有数据的采集，分析，存储和展式。只是具体收集的数据维度不同，分析过程不一样。

tracing是一期的内容，本次调研用到的各组件都是有机会在其它地方使用的。这些轮子用熟之后，二期可以做更多监控方面的东西。

我们的目标是--让我们的基础设施更加完善和强大！

# 参考资料

1. google的大规模分布式系统的跟踪系统[Dapper](http://bigbully.github.io/Dapper-translation/)，经典论文
2. twitter的[zipkin](https://twitter.github.io/zipkin/)，开源，scala的，装不上
3. 淘宝的[鹰眼](http://www.slideshare.net/terryice/eagleeye-with-taobaojavaone)技术分享PPT，干货!
4. 窝窝网介绍Tracing的一篇[博客](http://www.cnblogs.com/zhengyun_ustc/p/55solution2.html)
6. 唯品会[Microscope](http://blog.csdn.net/alex19881006/article/details/24381109)
7. [一个老外写的PPT](http://surge.omniti.com/2014/presentations/CrossStitch_PaulWright.pdf)
8. [埋点要用到的轮子](https://github.com/spacemonkeygo/monitor/trace)
9. [Kids](https://github.com/zhihu/kids)知乎开源的日志聚合系统
10. [介绍heka的一个PPT](https://people.mozilla.org/~rmiller/heka-intro-2013-03/#/)
11. graphite，Scalable Realtime Graphing
12. [InfluxDB](http://influxdb.com/)是一个开源分布式时序、事件和指标数据库
