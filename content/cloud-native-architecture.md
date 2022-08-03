在21年的总结里面我提到了，周sir 发过一个关于云原生架构设计的内部文档，当时读完了感触是，这个架构设计得是真心牛B呀！当时还问周sir 在搞啥，原来是在憋大招。算是我们下一代架构的原型，读完就跟读到一篇非常棒的论文一样的如沐春风，斗胆写一写点评，都是个人理解。

## 解决存储成本问题

首先是副本数量。我们需要 3 副本是因为，节点可能会挂，数据可能会临时不可用，持久存储也不是真正的稳定可靠的，盘可能会坏掉。然而，3 副本也同样意味，存储成本放大了多倍。

产品上云之后，基础设施变得不太一样了，云上面会有弹性的计算节点，有"可靠"的存储节点。假设我们已有了可靠存储的情况下，比如说 aws 的 s3，能否不再存储 3 份副本呢？ 这是架构推衍的第一步。

然后思考一下数据存储的形式是什么。SQL 数据库的行/列/索引，这些实际上都编码成了 kv，所以存储引擎到下层都是 kv 存储。以 rocksdb 为例，**最终持久化的是什么？是 SST 文件**。所以我们希望 1 副本，就是把 SST 文件都存到 s3 里面，只存一份，这一份 SST 就成了 single source of truth。

顺着下一步推衍，假设只有一份 SST 并且存到 s3 里面了，那存储引擎每次都从 s3 去读数据么？ 那不是相当于本地的存储引擎，挂载一个远程的盘。为了提升性能，我们似乎需要一个缓存层，SST 文件在 s3 存一份，在本地缓存一份。 **本地的 SST 是一份缓存**，这是下一个很重要的设定。

为什么 SST 是可以缓存的？ 方便缓存的条件是，只读不修改，而 SST 正是这样的。LSM 是把可变的部分放在 memtable，然后周期性的做 compaction 把 memtable 往下层刷，生成 SST。SST 一旦生成之后，在下次做 compaction 之前就不会被修改了。

写放大会变小。表面上看，SST 存在 s3 之后会引入额外的网络同步的开销，但实际上写放大是变小的。因为以前 3 副本，各自做 compaction。而**将本地数据变缓存，直接数据只存一份到 s3 之后，做 compaction 可以只做一次，其它节点去同步 compaction 之后的结果**。

## 更好的弹性，以及性能抖动变低

compaction 可以挪出去在远程做，因为数据放在 s3 了。或者说，前台任务跟后台任务分离了，如果发挥云上弹性计算节点优势，这种情况可以起一些计算节点去做后台 compaction，存储引擎节点不会引入性能抖动。

计算量减轻。做 compaction 不仅是写放大的影响，还会消耗很大的计算量。compaction 可以占到 CPU 的 30%，然后性能抖动也经常发生于 compaction：CPU 打满之后请求排队，磁盘写带宽耗尽之后 stall。

除了 remote compaction 之外，数据库里面类似的这种造成抖动的 周期性任务，比如 analyze，比如 ddl add index，这一类的都可以搬出去成为单独的服务，用动态弹性计算节点做，不影响原始计算节点的服务质量。这都意味着更好的弹性，发挥云上面的架构优势。

## 解决 LSM 的扩展能力

在我们的当前使用姿势下，rocksdb 的引擎其实是有很多瓶颈的。

写放大已经说过了，LSM 的层数越多，写放大问题也就越严重。因为 compaction 的时候每往下一层，涉及的数据量会增大好多倍，于是两层合并写数据的量也就跟着放大多倍。**假设我们不是单个大的 LSM 支撑一个 tikv 节点，而是拆小成多个 LSM，数据量减少后，LSM 层数降低，写放大也会变小。**就类似于做排序，做全量的复杂度还是 n^2，而用归并排序方式，拆小数据集，再合并结果，整个复杂度就降成了 nlogn。

region 过多。因为在此之前，tidb 是 96M 一个 region，然后 region 是逻辑概念，跟物理解耦的。物理上是每个 tikv 对应一个 LSM，然后许多 tikv 来存全部的 region。region 过多之后，raft 的心跳会造成很高的 CPU 消耗，集群数据达到 T 量级，4c 8c 这种配置单是处理 raft 心跳就要消耗掉 30% 的 CPU。所以 tikv 做了 hibernate region 这类的 feature，然而问题还是没被完成解决，引入了更多复杂度。如果把 region 跟 LSM 逻辑物理统一起来，不再设置 96M 一个 region 的限制，那么 region 过多的问题就可以解掉。

性能抖动。compaction 那一块说过，还有一些后台任务的，就不重复了。另外一个性能抖动的来源，是 multiple-raft 的 region 分裂和调度的时候，搬数据的问题。这里面有一些 corner case 比较恶心。在新的架构下，搬数据变得无比简单，因为本地 SST 都变成缓存了，而 single source of truth 的数据都是在 s3 里面，没有复杂的逻辑了，只需要把 SST 复制过去，然后更新 LSM 的层级的元信息。

raft log 的优化。其实分布式存储就处理两块内存，一块是 log，一块是数据。log 就是走分布式协议达成一致后，记下所有操作的序列。数据就是 apply log，将操作变成快照。log 是很短时存在的，它的处理模式数据不一样。当 log apply 成快照之后，log 就可以清理掉了。我们用 rocksdb 存 log，这个姿势其实是不太好的，因为 LSM 这种结构，清理数据靠的是 标记清理+ GC compaction 的模式，写放大是特别高的。重新设计架构的话，raft log 这一块引擎可以定制。

## 对比 Aurora 架构

Aurora 是一个很典型的 cloud-native 的架构。它其实是共享了从日志到存储层，而上层则复用 mysql。
这个架构让它可以很方便地 scale reading 能力。新拉起一个实例，直接起来就可以服务读了，弹性也特别好。
比较不好的地方，是写的扩展能力。基础的设定是一写多读，扩展写能力，得靠升级写的节点的机器配置，属于是纵向扩容。

"下一代架构"设计里面，相比于 Aurora，写能力是可以 scale 的，记 log 不是由单个节点完成，而是走分布式协议。log 的顺序问题由 multiple raft 来切小粒度。在读的能力，以及弹性方面，达到了跟 Aurora 一模一样的扩展效果。新起一个实例 -> 从 s3 加载 SST 到本地 -> 实例开始对外服务。等于是把 Aurora 架构里面最有价值的地方"借鉴"过来了。

本地 SST 变成缓存后，有什么好处？

首先，我们要知道，kv 存储，其价格是比 块/文件存储 的价格，贵很多倍的（不信随便对比一个公有云服务，去看它们卖数据库存储的价格，跟卖块存储或者文件存储的服务价格）。因为 kv 存储在一致性，持久化之上，还会要求高并发，低延迟。而块/文件存储，只有持久性和吞吐的要求，对响应延迟要求是降低的。那么举个例子，存储介质 kv 则至少 ssd 起步，或者到 optain 甚至内存。而块/文件存储，用 sata 做 raid 也能用。

LSM 中一些比较核心的概念：WAL + MemTable + SST，记 log 是为了防 crash，然后 MemTable 到 SST 我们可以类比成机器硬件中一级缓存，二级缓存这种概念。到了云架构中，将它变成 WAL + [MemTable + SST(缓存)] + SST(s3)。看！我们把一级缓存，二级缓存，扩充到了三级缓存！然后云存储成了最终的存储介质，本机上面 SST 成了缓存部分。

我们通过这几层缓存，把 kv 类型的 IO，变成了块/文件存储的 IO。结果是，我们在云的架构上，得到了 kv 的服务质量的能力，而付出的成本只是略高于块存储/文件存储服务的成本。这里面的核心秘密就是本地 SST 这一层变成了缓存层。

## 与 cockroachdb severless 的对比

友商的 [How we built a forever-free serverless SQL database](https://www.cockroachlabs.com/blog/how-we-built-cockroachdb-serverless/)。

一些设计点列出来：

- SQL 层每个租户是自己的 SQL 层
- 存储层共享同一个，在 key 的编码里面添加了 tenant id 来划分，做好访问控制
- 对 kv 层，要做好 tenant 级别的读写请求的 measure
- 进程的 CPU 和内存的使用量... 通过 cgroup 搞定
- 什么时候应该扩缩容？
- 服务质量保证
- 元数据管理

做的这些事情也是一个云上数据库的架构设计。跟周sir 提的这一个设计专注的领域不一样。

周sir 主要是在 kv storage 服务那层的架构设计，不涉及到上层的计算层。
友商在 kv storage 层做了定制的 pebble 替换 rocksdb 引擎的事情，只是单机那一层的优化，而没有完全结合到云原生的能力，格局上还是逊色一筹。

至于多租户的设计，我们可以抄过来，好东西就应该多借鉴 :-)

## 一点感想

对 cloud native database 概念的理解：在架构设计上，充分考虑云技术特点的分布式系统。

扩展性 -> 弹性

以前需要预估资源的使用量，而在云原生的架构设计下，弹性非常好。需要多少资源不再预估，不够就动态地加。

隔离 -> 支持多租户

pay for what you use ->  对资源使用的精确控制能力

价格是基于资源使用的，不使用时就不花钱。instead of pay for server, pay for request!

容错能力 -> 充分复用现有资源

成本

备份恢复
