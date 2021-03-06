TiDB里面有个很恶心的lease参数，这个参数设置小了，TiDB就会挂掉。但设置大了呢，做DDL的时间就特别长。用户就很困惑，为啥有这么蛋疼的参数。
被问了很多次以后，我们将它放到了FAQ里面：

> TiDB 的 lease 参数应该如何设置？
>
> 启动 TiDB Server 时，需要通过命令行参数设置 lease 参数（--lease=60），其值会影响 DDL 的速度（只会影响当前执行 DDL 的 session，其他的 session 不会受影响）。在测试阶段，lease 的值可以设为 1s，加快测试进度；在生产环境下，我们推荐这个值设为分钟级（一般可以设为 60），这样可以保证 DDL 操作的安全。
>
>
> 在使用 TiDB 时，DDL 语句为什么这么慢？
> 
> TiDB 实现了 Google F1 的在线 Schema 变更算法（具体参见 F1 论文 和我们的一篇 Blog）。 一般情况下，DDL 并不是一个频繁的操作，我们首先要保证的是数据的一致性以及线上业务不受影响。一个完整的 DDL 过程会有 2 到 5 个阶段（取决于语句类型），每个阶段至少会执行 2*lease 时间，假设 lease 设为 1分钟，对于 Drop Table 语句（需要两个阶段），会执行 2*2*1 = 4 分钟。除此之外，DDL 的时间还取决其他的条件，比如做 Add Index 操作时，表中已有的数据量是影响 DDL 时间的主要因素。我们也了解过 Google 内部在 F1 上是如何做 DDL，一般是提交给 DBA，DBA 再通过专用的工具执行，执行的时间会很长。

然而这并没什么卵用。

首先，这个解释太技术化了，普通用户根本难以理解。

其次，小白用户根本就不看文档的，所以解释再多也白搭。

写这篇文章，是最近要做一个优化，把等待时间干掉。lease参数就不应该暴露给用户。

----------------------------

还是先讲一下online schema change，主要提一下思路，具体的细节可以看[论文](https://research.google.com/pubs/pub41376.html)就行。

schema是数据库的元数据，一个数据库的表结构是什么样的，有哪些列，列的类型是什么，有没索引，这些东西都是schema信息。问题是这样子的，在一个分布式的数据库里面，并不容易保证大家看到的是同一个schema。如果schema有更新了，假设有一个节点用的旧的schema信息，而另一个节点用的新的schema信息，数据的一致性就难以保证了：比如某个表添加了一列，使用旧的schema插入数据，插入的数据没有这一列，而使用新的schema的节点，插进去的数据有这一列，显然是有问题的。

online schame change并不是一口气从旧的schema更新到新的schema，而是拆成了好几个阶段：可删除、可写、重组、可读。并且保证**任意时刻所有的节点最多看到两个不同状态**。

下面我们以添加index为例子来说明，注意考虑如果没有中间阶段会出现什么问题。

## 可删除

我们假设schema更新了，而有个节点用的仍然是旧的schema，也就是它不知道这个索引，那么它执行DELETE会生什么事情？它会把数据删掉，但是却没删掉索引，于是出现“悬挂”的索引，这个索引指向了一块不存在的数据。

可删除阶段的引入就是解决上述问题。处于这个阶段，对新添加的索引只能执行删除操作，不能做其它操作。

## 可写

假设schema有更新，有的节点用的旧schema，有的节点用的新schema，执行INSERT操作会出现什么问题？旧的节点插入了数据，却没有插入索引，这条记录是不完整的。

可写阶段的引入就是解决上述问题。在这个阶段，可以写入索引，也可以删除。但是无法执行其它操作（索引还不可见，无法读）。

## 重组

这个阶段执行的操作非常重要。经过前两个阶段，我们知道，整个数据库的状态可能是，有数据但是却没有索引。在这个阶段里面会做backfill， 给旧的数据把索引填充上去。
这样子在进入下个阶段之前，我们可以保证，数据和索引都是一致的。

## 可读

到了这个阶段，新加的索引才会变成对外可见。

上面就是大致的流程了，虽然是添加索引为例，但是对于其它的DDL操作比如添加删除列之类的，都是类似的走这几个阶段。

---------------------------

这个算法最大的问题是什么呢？

为了保证算法的正确性，有一个严格的约束条件：任意时刻所有的节点最多看到两个不同状态。实现这个约束，采用了[lease机制](schema-lease.md)，导致等待时间过长，也就是文章开头那一堆问题blahblah。

1. 从一个版本变更到下一版本，必须等待1个lease时间。
2. 客户端必须在lease过期前拿到最新的版本。如果拿不到，当前版本在超过2个lease就过期了，不提供服务了。

这样子，前者保证了一个lease之内，整个系统不会存在两个版本。后者保证所有节点对一个版本的使用不会跨越两个lease。共同作用之后，就保证了任意时刻所有的节点最多看到两个不同状态，约束条件成立了。

其实呢，仔细想一想，并不一定非得走lease机制。以前等lease的行为无非是为了保证约束条件。

实际上用什么方式实现都行，只要能保证约束。所以我觉得优化要解决问题就只有一个：

    若所有的TiDB能对最新的schema版本达成共识，就不用等lease了。
    
前段时间我们引入了etcd，引入etcd之后就有更大的想象空间了。以前由lease保证的事情，可以etcd这边维护一致性，可以主动地做一些事情。

以上。
