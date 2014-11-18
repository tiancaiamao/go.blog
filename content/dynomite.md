本文来自[netfix的dynomite介绍](http://techblog.netflix.com/2014/11/introducing-dynomite.html)，不是完整翻译，而是阅读摘要记录。

# 服务器架构

## 动机
现在有很多单服务器的数据存储方案，像memcached,redis,berkeleydb,leveldb,mysql。最终可用性的解决方式都是做主从。当请求量大到一定程度，都是做分片。这些操作都很麻烦，由应用开发者都维护分片更是蛋疼。

Dynomite的目标是将这些单服务数据存储做成p2p的，线性可扩展的集群系统，仍然保留原生的协议，比如redis。

## 拓扑

一个集群有多个数据中心。一个数据中心有一组机架(rack)，一个机架内有一组节点。每个机架都有整个的数据集，分布到架内的不同结点中。机架内的每个节点都有一个唯一的token，用于确定它负责哪些数据集。

![](https://lh3.googleusercontent.com/ll4DYmppK2Cs_xWUczAg9DWz7cUCFaxgkjprEDcksAB6_38B6uBZlx7eI9BqNehQ3KiIcKiT35CAPcKGuAd4dIvy_rC6H4qMBXZjU7-K1EjoDaBOsAHG_uagpepwSnUaRw)

每个Dynomite节点由一个Dynomite进程和一个数据存储服务器绑定，Dynomite进程作用是代理，路由请求，协调和gossiper。

![](https://lh6.googleusercontent.com/C2P4uE7WVURiw1aE0aUcaWXiSNZU6mqIxSmg-c8CL2sdbqbSTZEAA-6gUo1XO59KalA-waVsnpnQ4xfjTnDdEX9atlmXK6Wc8bxQKPkZeOy8R9HQ0GwAO1jtSDCn6s0RjQ)

数据存储服务器可以是memcached或redis这类不持久化的，或者像mysql,berkeleydb或leveldb这类持久化的。当前Dynomite支持redis和memcached。

## 副本

客户端可以连到Dynomite集群中的任何节点发送写请求。如果正好该节点负责这份数据，那么数据会写到本地存储并异步同步到所有数据中心的其它机架。如果节点不负责这份数据，那么它作为协调者将数据转发到同机架的节点。它还会将写请求发给对应机架和数据中心的其它结点。

当前是写到本地机架就像客户端返回ok，写其它副本是异步的。

## 高可用读

多机架和多数据中心提供了高可用。客户端可以连到任意节点上去读数据。跟写类似，如果节点上没有数据，它会转发读请求到同一机架中拥有数据的节点。容错性方面，客户端可以请求其它机架或数据中心的节点。

## 可插拨的数据存储

当前支持redis和memcached。大多数的api子集都支持。

## 支持标准的memcached/redis协议

任何memcached或redis的客户端都可以直接连dynomite，不需要改。但还是缺一些东西，像自动容错，请求流量限制，连接池之类的。这些在Dyno客户端中有。

## p2p

集群内的Dynomite节点角色都是相同的，因此没有单点问题。加节点可以直接加。

## 恢复

空节点可以从其它节点从拿数据，加快恢复时间

## 异构的数据中心副本

写会被复制到多个数据中心。不同数据中心可以配置成不同数量的机架和节点数。如果不同数据中心请求量不均衡，这个feature特别好。

## 节点内部通信

Dynomite内置的gossip维护集群成员关系并处理选主的恢复问题。

# 客户端架构

普通的redis和memcached客户端都是可用的(前面提过服务器节点转发)。Dynomite也有提供自己的客户端Dyno。有些feature。

* 支持连接池
* 拓扑aware以及负载均衡
* 应用可指定本地机架
* 智能容错(本地节点或机架挂了)
* 监控连接健康状态，回收有问题的连接
* 当结点下线或维护时可以处理路由
* and so on...