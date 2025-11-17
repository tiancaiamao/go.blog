利益相关：作者是做 TiDB 的。TiDB 发了一篇 VLDB，CRDB 发了一篇 [SIGMOD](https://dl.acm.org/doi/pdf/10.1145/3318464.3386134)，VLDB 和 SIGMOD 都是数据库顶会，从这个角度看谁在这个领域发展势头上跑得快一点，应该说不相伯仲。只讨论技术，不引战。

两者论文的创新点对比，TiDB 说自己开启了一个新的 HTAP 时代，CRDB 的卖点是 Geo-distributed 全球同服务器。

## 第一部分：架构

架构方面，按逻辑分层，划分了： 

SQL 层，整个架构的入口层，负载将外部进来的 SQL 查询转化为内部的事务。

事务 KV 层，这一层做的事情也就是在 MVCC 上面构建事务。

Distribution，其实是分布式系统下的数据分片策略。简单讲是按 key 的 range 分的，每 64M 构成一个 range。

Replication，这一层其实说的就是 raft。Distribution 和 Replication 对应于分布式系统里面的术语，解决的问题就是分片和副本，这是分布式系统下非常基础的东西。每个 64M 的 range 有一个 leader 多个 follower，对应一个 raft group。整个系统里面会有许许多多这样的 raft group，为了减少消息通信，节点会把多个 raft group 的消息会合并，也就是 CRDB 所说的 multi-raft。

存储层，直接使用了 RocksDB。

架构的分层其实跟 TiDB 是一样的。比较小的区别是 CRDB 提供单个 binary 来做所有事情，而 TiDB 把计算和存储层放到不同的 binary 了。这一点有好有坏吧，单个 binary 好处是方便 deploy。坏处是复杂度变高了，论文后面的经验教训部分有提到，单个 binary 在滚动升级时会出现集群同时存在新旧版本的 binary，这个场景容易搞出问题来。

### 容错和高可用

leaseholder 就是一个 raft group 的 leader。这里讲一个背后的小故事。其实它们以前不是一个概念，raft leader 是 raft leader，而 leaseholder 是 leaseholder。由 leaseholder 负责接入进来的流量处理，而 leasehodler 实际上还是要把请求发往 raft leader 处理。它们希望，leaseholder 和 raft leader 尽量是落在同一个物理节点上面的，这样子这一层转发的开销就比较低。这样设计的理由是什么呢？我推测应该是 raft leader 会漂，比如漂到其它机房去了。这个时候它还是希望处理客户请求的，还是在本地机房，所以它要把 leaseholder 和 raft leader 概念分开，再等 raft leader 迁回来。最后 CRDB 改掉，让 leaseholder 跟 raft group 的 leader 是同一个概念了。

TiDB 其实也有类似的问题，etcd 的 leader 和 pd 的 leader 不是同一个 leader。如果 etcd 的 raft leader 漂到了另外一个机房上面，这时就会抖一下。

leaseholder 要 hold lease 需要维护心跳。节点将心跳信息记录在一个特殊的 system Range 里面。leaseholder 每 4.5s 心跳一次，system range 过期时间设置为 9s。

这个设计我们看一下，心跳是放在一个 system range 的，那么这个 range 挂了就会整个集群的影响，可能导致不可用。可以看出来，它们是做到自举了，由自己的 raft 来服务自己的元信息存储。优点就是 eat one's own dog foot 很优雅，缺点就是挂了就挂了。TiDB 也有存元信息的，元信息没有自举，TiDB 是用的 etcd 的，就引入了外部系统依赖。

**如何保证同一时间，只有一个副本是 leaseholder 的呢？**

leaseholder 是实现在 raft 协议上面的。如果一个副本想成为 lease holder 它需要提交一个特殊的 lease acquisition 的 raft log。

**如何保证两个副本获取的 lease 时间不出现重叠？**

这个没有看得特别明白。能理解的是，lease 的时间不能是基于机器的本地时钟，因为时钟会漂移，如果基于本地时间，可能有些节点认为自己还在 lease 以内，而另一些认为它已经过期了。

关于高可用的，这块就是靠的 raft。每个 raft group 维护三副本，如果有其中一个挂了，自动做 membership change 让 raft group 恢复。

### 副本放置策略

- Geo-Partitioned Replicas 表的每个 partition 分到特定的区域。整个区域挂掉会导致相应的区域数据不可用，同一区域内部的读写很快，并且可容忍 AZ 级别的故障。
- Geo-Partitioned Leaseholders 把请求和 leaseholder 放在同一个区域，把副本放在另外的区域。可以容忍整个区域挂掉，代价是跨 region 写入比较慢。
- Duplicated Indexes 在靠近数据访问的地方，放冗余的 indexes 的 leaseholder 副本。这样走本地索引就很快。缺点是，额外副本的写放大，以及跨 region 的写变慢。适合频繁读但很少更新的场景。

对于副本放置策略这块的看法，个人感得 CRDB 非常强调 geo-distribution，在不同的策略下，对可用性和延迟等等的取舍。

## 第二部分：事务模型

### pipeline 和 parallel commit

首先是两个优化，pipeline 和 parallel commit 我分别在[去年](/cockroach-parallel-commits.md)和[前年](/cockroach-transaction-pipeline.md)写过博客。尤其是 parallel commit 这个是去年看过的最最精彩的技术文章。


pipeline 跟 CRDB 模型写 write intent 有关。接受请求的节点叫 coordator，coordinator 在 TiDB 里面，对应的就是 tidb server 的角色。然后 coordator 转发到 leaseholer，leaseholder 再做 replication 走 raft，成功了之后返回。pipeline 这个优化是说，不用走 raft 了，走到 leaseholder，然后 leaseholer 计算完影响了多少 row，就直接给返回。

读很快，写比较慢些，因为有 leaseholder 的情况下，读操作 leaseholder 可以直接返回结果给 coordinator 的，写操作则需要同步副本。

再简单的说下 parallel commit。

我们基于这个观察：为什么需要两阶段提供？因为涉及到多参与者的时候，有可能部分成功部分失败，而通过两阶段提交保证第一阶段提交成功之后，第二阶段不可能失败。
那第一阶段做什么事情？做持久化，只要数据持久化了成功，后面第二阶段就不会失败了。再类比 raft，写多数副本其实就是做持久化，多数副本成功了后面就不可能失败了。

两阶段提交的 prewrite，跟 raft 的 replication 两种其实本质是一个事情。于是并行提交的本质优化是在做啥？**第一阶段跟 raft replication 其实是等价的，只要持久化了，操作之后不可能再失败了，就可以返回客户端**，基于这个观察，它引入了一个 staging 阶段，可以不必等到 commit 阶段完成，才去返回客户成功，而是在能保证 raft replication 不会再失败之后，就返回客户端，省掉了一轮等待。

parallel commit 这个优化，对于 latency 的降低效果太高了，按论文中的说法，吞吐提升了 72% 并且将平均延迟降低了 47%。


### 原子性

**MVCC + 单个事务状态 key 保证原子性**。整个过程都是 MVCC 的，然后每个事务只有一个唯一的 key 来最终决定事务的状态。在 TiDB 里面其实也是一样的，之前我也写过相关的[话题](/tidb-transaction-internals.md)。

CRDB 里面，事务的状态为 pending staging committed 或者 aborted。这几个状态变迁都是在那个 key 上面的原子性变迁。

如果遇到了 write intent，write intent 里面记录了事务的记录的 key，根据这个找到事务状态，再决定相应的操作：

- 如果是 aborted 就清理，读 MVCC 前一版本的值
- 如果是 committed 就是读到这个 write intent 的值
- 如果是 pending 那么 reader 要等待事务完成。如果 coordinator 节点挂了，那事务的 ttl 就不更新了，可以做清理。
- 如果是 staging 状态要么是 commit 了要么是 abort 了，但是 reader 不知道是哪一种。这个时候尝试发 abort，

### 隔离级别

TiDB 默认是快照隔离，而 CRDB 是可串行化。**在事务模型上面，一个很重要的区别是，TiDB 的 startTS 和 commitTS 是两个，而在 CRDB 里面事务只有一个 ts**，既是 start 也是 commit。

先看几个场景。

第一个场景是，一个事务的写操作先执行，然后另一个事务的读操作到达，读遇到了未提交的 write intent。如果读的事务 ts 较小，则可以直接跳过 write intent 读上一版本。如果读的 ts 较大，则需要等待，也就是读事务要等写事务完成。

第二个场景是写写冲突。写会留下 write intent，后到达的一个写会遇到前一个写的 key 的 write intent，也是可以根据 ts 做相应的处理的。

写写冲突没有可避免的。如果上一个事务的 ts 较小，刚等待前一个事务完成。如果上一个事务的 ts 较大，说明冲突了。

注意一下死锁的处理。这里提一个问题：分布式死锁是怎么做的？论文中在这一块是一笔带过了。

其实如果把死锁检测真正做成分布式的，这个算法是很难的。TiDB 里面本质上还是一个单机的死锁检测，只是用了一些工程的手段，保证了死锁检测的单机挂掉的场景，整个系统是可恢复的。所以 CRDB 里面，如果它是用 leaseholder 搞这个事情，那还是单机的死锁检测的搞法，没看过代码确认。而如果它是一个"真"分布式死锁检测，那就很牛B了。

第三个场景是，读操作在先，写在后面，这是最复杂的一种，需要展开讨论。

分布式系统里面，消息到达的顺序是完全不能保证的，就像一个先发生的事件，经过漫长的网络，后到达了接收者。**当一个写操作到达时，可能有比这个事务的 ts 更大的事务，已经读过旧的值**，这时一旦执行写操作，会导致前面读事务没读到正确的数据，因为读已经发生了并且没有读到这个写。必须要阻止这种情况发生！

那么首先应该想到，如何让写操作能感知到读操作？这是一个难点。写怎么知道有没有人读过，因为读操作是不会留下 read intent 的。没有 read intent 这种东西的。写操作怎么保证，自己的 ts 是大于所有读操作的 ts 的？

这里涉及在内存层加锁，也就是 leaseholder 在 latch 那里的处理。读操作需要把 key 的最后一次的读 ts 记录下来。写操作在获取 latch 的时候，如果发现操作的 ts 小于 latch key 的记录的最大读 ts，就能检测到。

这里如果细想的话，问题还挺复杂，leaseholder 可能会迁移，那么迁移之后，内存 latch 上面的信息是不是会丢失，该怎么处理？
通过 HLC 的时钟，能保证修改的 key 有交集的事务之间的一致性，是否需要保证写事务的 ts 比所有当前读事务的 ts 更大？？

第一步是检测，第二步是检测到之后，该做什么处理 -- 推 commitTS。

写事务在检测到上面的场景以后，要重新刷新 commitTS。刷新 commitTS 之后，写操作就可以看作发生在前面的读操作之后了，于是就不会影响前面的读。

接下来，推完 commitTS 又会发生什么事情？

事务的 commitTS 变化之后需要保证，它以前读过的数据依然是有效的。因为可串行化隔离级别需要满足可重复读，也就是要求，用推之前的 ts 跟推之后的 ts 再读一遍，读到的数据是没有变化的。

如果数据变了，这里又有一个细节，涉及跟客户端交互协议。如果前面的读已经给客户端返回过数据，那就只能失败了。如果还没有给客户端返回任何结果，那么是可以事务整体重试的。 

那么怎么知道推 ts 前后，读过的数据变没变呢？CRDB 把事务读过的结果集都先存在内存了，然后用新的 ts 再读一遍数据，再做对比看变没变。这个过程等价于检测读写反向依赖。

还有另一种情况需要刷新 read ts，当遇到值的 ts 落在不确定时间范围的时候，不确定区间在下一部分时钟同步会讲。

## 第三部分：时钟同步

paper 的 Section 4，一整章节花了很大的篇福来讲时钟，这个 Section 也是 geo-distribution 的关键部分。先说一下背景，为什么需要时钟？为什么 TiDB 不支持 geo-distribution？

在前面的事务模型里面，决定事务谁先谁后，是通过比较 ts 来确定的。然而这个 ts 不能是一个机器的本地时间，因为机器的本地时钟会漂移，会出现事件 1 发生在事件 2 前面，但是得到的事件 1 的 ts 大于事务 2 的 ts，如果这两事件有逻辑上的交互，就会违反因果一致性。为了解决这个问题，TiDB 采用的是中心时钟，由 PD 组件提供，事务要去 PD 获取 TSO 来保证正确的时序。由于采用了中心化时钟，取 TSO 会受到网络开销的影响，比如说一个节点在欧洲，一个节点在北美，这个延迟就太高了，所以不能很好支持 geo-distribution。

Spanner 的 TrueTime 是用的特殊硬件，保证了不同机器时钟间误差的上限，结合 Spanner 的事务算法，它也是可以用本地 ts 去比较先后顺序的，因此也是支持 geo-distribution。

CRDB 的做法是用 HLC(混合逻辑时钟)，它的创新之处在于可以不依赖特殊硬件，支持 geo-distribution。

### 混合逻辑时钟 HLC

混合逻辑时钟其实[以前我也写过](/hlc.md)，现在再写一遍。混合逻辑时钟存储两部分信息，一部分取值来源于本地时钟(物理部分)，另一部分取值来自于计数器(逻辑部分)。

来源于物理的部分，里面保存的其实是当前所有参与节点的本地时钟的最大值，另一部分则是每次事件或者消息通信时加加，类似逻辑时钟里每次事件都加加。

HLC 的性质：

可以捕获因果关系

这里所谓捕获因果关系，指的是如果事件 A 发生，导致了事件 B，那么 A 的 HLC 一定是小于 B 的。

> if e happens before f, then HLC[e] < HLC[f]

举个例子，机器 A 向机器 B 发了一条消息，那么，机器 B 收到消息的时间戳，肯定是小于机器机器 A 发送消息的时候戳的。每条消息里面都会带上 HLC 的时间戳，而节点每接收到消息都会更新自己的本地 HLC。

可以跟物理的时间关联起来

捕获因果关系不需要 HLC，用普通的 Lamport 时钟也可以做到。那么 HLC 比 Lamport 时钟的好处在哪里呢？Lamport 时钟是没法捕获全局快照的。

举个例子，整体集群内，有 A B C 和 D E F 这些个节点，A B C 发生通信，D E F 发生通信，但是没有交集的部分，我们没法确定一个快照时间，说明谁是先于这个快照，谁是晚于这个快照，因为没发生过通信不具备可比性。

HLC 是由物理部分和逻辑部分组成，其中物理部分来自于节点的本地关联的。这就产生了一个可比性的基础，如果有 HLC[e] < HLC[f]，我们可以假想一个 HLC[g]，它的物理部分是落在 HLC[e] 和 HLC[f] 之间的，它是逻辑部分为 0，这样 HLC[e] <= HLC[g] <= HLC[f]。那么我们假想的这个 HLC[g] 就可以作为一个快照时间。

HLC 是严格递增的，在 CRDB 里面，节点重启的时候，必须要等的节点的物理时间走到比之前的 HLC 更大之后，才能继续提供服务。这一点类似于 TiDB 的 PD 会周期性的把分配出去的 tso 记录到 etcd，节点切换或者重启的时候，必须等节点的物理时间走过了之前的最大值，才能保证新分配出来的大于之前最大的。

### 不确定区间

HLC 是一个偏序关系，也就是发生在前一定小于，小于不一定发生在前。发生在前面一定不大于，大于则一定不发生在前面，有点绕哈。

**如何比较两个 ts 谁先，谁后？**

需要引入一个不确定区间。假设用 HLC 给一个事务 A 打一个时间戳 ts，加一个不确定区间 `[ts, ts + maxOffset]`。如果另一个事务 B 的时间戳是小于这个区间的最小值的，那么 A 能看到 B 的结果没毛病，不破坏因果一致性。

然后说这个不确定区间的作用。

机器节点之间是有时间偏差，但是假设用 NTP 做校正，这个偏差会在一定的范围内，最多相差 maxOffset。那么我们取两个物理时间，如果事件 A 是 ts，事件 B 比事件 A 大了 maxOffset 以上，即使考虑不同节点的时钟偏差，我们依然是可以肯定：事件 B 肯定是晚于事件 A 的。

所以落在 [ts, ts + maxOffset] 这个区间之外的，是可以比较先后顺序的，而落在这个区间内的，则无法判断先后(更准确说法应该是判断 happens before 关系，并且不违反 causal consistency)。

**如果落在不确定窗口，怎么处理？**


事务在创建的时候，会分配一个 commitTS 和不确定的时间区域 `[commitTS, commitTS + maxOffset]`。如果遇到了一个 key 的 ts 比这个区域小，那么就是当前事务发生在后面。读可以读到那个值，写可以覆盖那个值。那如果遇到 key 的 ts 就落在这个区域之间呢？这就表示不能确定当前事务是在前面还是在后面的，处理方式是，刷新 commitTS 为一个比 `commitTS + maxOffset` 更大的值，就相当于把当前事务推到后面去。

CRDB 并不保证严格可串行化，这是因为假设两个事务，操作的是没有交集的 key ，没法通过 HLC 来判断它们的先后。可串行化是说，交叉的事务的执行结果，等价于**某种**串行执行的结果，这里等价于按 HLC 的大小顺序。严格可串行化，会更严格的要求最终的顺序与物理时间上的先后对应，HLC 并不能反映在物理时间上的先后。

### 时钟偏差超过 maxOffset

CRDB 要求配置 maxOffset，表示整个集群内节点之间的时钟偏差，上限不能超过这个值。前面的所有算法描述，都是基于不破坏这条约束条件的。

那么如果节点间的时钟偏差超过了这个值，正确性受到什么样的影响？

先看 raft，单个 key 的线性一致性是由 raft 保证的。raft 层没有 ts，不受时钟漂移的影响。但是注意，lease 这东西受时钟影响。lease 可以让 leaseHolder 不走 raft。如果时钟不同步，可能导致有多个副本都认为自己可以成为 hold lease 了，这会导致出现脑裂。

这条属性其中最重要的作用是，保证对每个 raft group，让每个 lease 负载的范围是正交的。

两条安全保证：

1. range lease 有 start 和 end 范围，lease holder 不能服务超出 lease 范围的请求。不同的 lease 区间是正交无重叠的。
2. 记 raft log 的时候，这个信息也带到了 log 里面。如果 replication 的时候 对不上，则拒绝写入。

再看对事务模型的影响。假设物理时钟偏差在 maxOffset 以内这条约束被打破，那么原来的根据 ts 加不确定区间，比较先后的做法就失效了。就有可能该读到没读，不该读到读了，stale read 这种。正确性就没法保证了。所以从结论上讲，时钟偏差超过 maxOffset 的时候无法保证正确性。


CRDB 的节点周期性的测量自己和其它节点之前的时钟误差，如果有节点相对于大多数节点，超过了允许的最大误差的 80%，则让该节点自杀。

一点疑问，既然 maxOffset 是配置的，到底应该配置为多少？NTP 能保证的时钟偏移的上限是多少？NTP 的对时准确性是否受到网络延迟的影响？

## SQL层

先留空


## 经验教训

Raft 方面的，multi-raft，静默 region。

配置变更的算法。

关于不再提供 snapshot isolation。它说性能差异很小。

为什么说在快照隔离下，保证强一致的唯一方式是使用悲观锁？也就是用 for update 和 for share，没看懂...

> The only safe mechanism to enforce strong consistency under snapshot isolation is pessimistic locking, via the explicit locking modifiers FOR SHARE and FOR UPDATE on queries

兼容性上面，客户端的自动重试行为

滚动升级，它们的滚动升级是一个 binary，就会导致有新旧版本共存的情况，可能引发严重的问题
