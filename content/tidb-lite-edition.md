很久都没有思考过，tidb 在架构方面的改动。因为架构改动都是伤筋动骨的，而现在的 tidb 已经越来越复杂了，改不动了。
最近跟同事有聊到这个点:如果我们回到当初创业阶段，重新来过，做一个精简版的 tidb，会有哪些变化。所以有一个契机思考这样的问题。先说结论是我没想到啥明确的结论，这篇博客只是头脑风暴把自己一些思考过程记录下来。

## ART (Adaptive Radix Tree)

首先想到的是一个 KV 数据结构，ADT(Adaptive Radix Tree)。也有一些内存数据库的核心数据结构是基于它的，然后 tidb 最近的版本中也有一个优化，是把 unistore 那一层从一个定制化的 rbtree 改成了 ADT。

传统的存储引擎中会用 BTree，或者是 skiplist+LSM 这种结构，反正不管哪种，总是需要一个 kv 的数据结构的。那为什么会想到 ART 呢？因为如果我们**只考虑内存 kv 结构的读写性能，怎么样得到最好的性能呢？核心问题其实是怎么样最好的支持并发**！
kv 的数据结构太多了，然后 hash 类的 O(1) 由于不好支持 range，要排除在外，最后算法复杂度就都是 O(logN) 了，如果纯考虑内存情况，关键点是在并发读写。而最容易并发读写的关键点是什么？是使用无锁数据结构，或者说 immutable，copy-on-write 这些关键词。只有尽量无锁，才能高并发，只支持高并发，才是纯内存角度下性能的关键。在单线程下，大家同样是 O(logN) 的算法。而在多线程下，假设写直接将读卡住，那么性能直接就 sb 了，锁是不 scale 的。只有无锁并发的结构才能 scale。

所以如果重新选择内存层的数据表示，我会想到 ART。

正好读到过 clojure 的 vector 的一篇博客，贴一个[链接](https://hypirion.com/musings/understanding-persistent-vector-pt-1)过来。跟本文内容没有直接关系，但是背后的关键点都是，什么样的数据结构，是最好的支持并发的: immutable 的数据结构。

## 以log为中心

以 log 为中心，这个基本上是所有云原生数据库系统达成共识了。在 Aurora 出来的时候就是，共享一套 log。我之前说过，在最近几年里面，读过的最好的内容就是周sir的[下一代云原生数据库设计](cloud-native-architecture.md)，这也已经是 tidb cloud 当前实际使用的架构了。

对于单机的数据库，log 就是 WAL 的 log。对于 tidb，就是 raft log，准确来说应该是 commit 操作对应的 log。只要把 log 的回放就可以得到原本的数据库快照，cdc 也是这样回放来的。只要把其中简单的 key range 的子集回放，就是 Aurora 某个租户的概念。
一旦到 log 这一层之后，就是确定性的，不会涉及到事务，没有冲突。

以 log 为中心，读和写就是分离的路径，读写解耦合，读可以无限扩展，因为从 log 中回放出多份副本，就支持读操作的 scale。

## tidb变成缓存

我们思考一下，怎么样得到最好的读性能？当 log 是所谓的 single source of truth 之后，我们还需要一个 kv 层，只不过它是相当于缓存，而真实的数据就是 log 本身。

SQL 层和 KV 层并合，也就是对于 tidb 的架构来讲，就是把 tikv 的 kv 层挪到跟 tidb 的 SQL 层在一起，这样就没有从 tidb 去访问 tikv 的一次网络开销。类似于 cockroachdb，它就是把不同的层，放到同一个实例里面的。我们如果把部署改成 tikv 跟 tidb 部署在同一个节点，也等价于可以消除这一层的网络开销。

假设我们是一个单机的数据库的时候，大概就是这种形态。假设有多个副本的时候，在另一个副本上面写入，那么 log 会不停地追加，如何保证读这个副本能读到最新的数据？我们可以在读之前，取一下最新的 log 的序列号到多少了，然后保证这个副本上面，log apply 已经大于等于该序号，就可以保证读到的数据没问题，snapshot read。这个取一个最新的 log 序号到了多少的过程，跟我们在 tidb 分布式系统中取 tso 也是本质上一样的东西，省不掉。

看到这里的时候，好像一切都很美好，我们只要把 pd / tidb / tikv 都单机部署，就相当于回到了一个单机数据库，访问网络或者取 tso 的开销都可以相当于忽略。
而且如果我们以 log 为中心，多个这样的数据库(pd + tidb + tikv)，共享着相同的 log (pd 的取 tso 变成取最新的 log 序号)，在 log apply 之后变成相同的副本，提供读能力，读性能几乎是完美等于单机数据库的。

那么为什么不这么干，问题是呢？

## replication和shard

分布式系统里面，非常关键的两个概念，分片(shard)和副本(replication)。

为什么我们需要副本？因为有可能分布式环境里面某个节点挂了，其它节点需要能够顶上，于是就需要副本保证可用性。分布式环境里面的假设，挂点都是随时会挂的，不能让整体系统变得不可用。多副本就引入了多副本的一致性问题，于是引入了共识，引入了 paxos 和 raft 这些。
还是先回到前面很美好的假设: 以 log 为中心，通过 apply log 就可以提供数据库的副本快照，读性能无限扩展还没有延迟的损失。但是这只提到了副本的概念，却没有提分片的概念！

假设数据量很小，所有数据都是单机能承受的，那么这么干就可以了。然而现实是骨感的，为什么会需要分布式，会需要分片？因为数据量太大了放不下呀。而一旦涉及到分片之后，前面美好的假设瞬间就都没有了。

于是每个 tikv 只存储了一部分的 key range 的数据，由许许多多的 tikv 共同完成全量的 key range。而 tidb 则需要访问许多个不同的 tikv 的分片，才能拼凑出一个查询请求的结果。

分层的好处是，tidb 跟 tikv 不需要 1:1，tidb 是无状态，也不缓存数据的。由于分层，带来的开销是，查询请求 tidb，数据都不是在本地，网络带来很大的影响。以 update 为例，update 之前往往需要知道之前的数据是啥，需要基于之前的数据修改，于是这里需要先从 tikv 读取一次，然后才能写入。更悲催的是 index lookup 这样的场景，先读 index 需要一次网络访问，而拿到 index 结果之后还需要再次回表访问 tikv 才能得到数据，这相对单机数据库的性能就是两次网络，两次磁盘读。而在单机数据库中，不需要网络，甚至有可能磁盘读都只有1次，比如说 index 在内存的话。所以当分布式之后，引入的延迟差异特别明显。

所以结论是，**replication 不影响读性能，但是 shard 非常影响性能**，而对分布式数据库的场景，shard 是不可避免的。

读的影响其实还是小头，要说对写入的影响，分布式(分片)就更严重了。

## 2PC 能否去除

写入路径跟读路径不一样，**由于有副本，我们需要有 raft。由于有分片，我们需要有 2PC。这两者叠加到一起后，延迟拉满**。

以 tidb 为例，让我们分析一下，一次事务提交的开销。

- 首先，由于需要有全局的事务定序，每个事务要从 pd 取 start ts 和 commit ts，这是两次网络开销。
- 然后2PC 的事务，它是涉及 prewrite 和 commit 两个阶段的操作的，我们先看 prewrite。
- 从 tidb 发送 prewrite 请求到 tikv，这是一次网络开销。
- 接着，tikv 收到 prewrite 请求之后，它需要写一些 kv 的 key，这至少是一次磁盘。
- 由于每个 key 对应 region 都是有多副本的，所以要走 raft 协议。raft 协议需要同步多副本并且收到多数派应答，这个是请求是并发的，我们只把它算成一次网络，实际的耗时会比一次略久一点，因为有的节点快，有的节点慢。
- 然后 raft 的协议细节中，副本在做应答也是要写 log 的，这又是一次磁盘。
- 所以整个 prewrite 过程至少是两次网络，两次磁盘的。
- commit 的过程跟 prewrite 过程基本是一模一样，就不用分析细节了，也是两次网络，两次磁盘。

oh my god! 整个过程算下来，至少是 6 次网络，4 次磁盘的，不知道我有没有漏掉啥的。所以 2PC 这是慢的根源啊！分布式事务的写路径太复杂了，延迟不高才怪了。

所以我想，能不能绕过 2PC 呢，于是我想起来之前读过的一篇论文，它是跟 Spanner 同年发的，采用的完全不一样的思路，它就是 [calvin](calvin.md)。

## 再看 calvin

calvin 采用完全不一样的思路是，它不采用 2PC，而是提前给事务确定性地排序和检测冲突。这样它就没有 2PC 的代价了。相比 spanner 或者 tidb 这种，它多出来了事务排序和冲突检查那一层，做完这一步之后，事务的提交就是确定性的写入，写入到 log 了。

calvin 具体的如何实现确定的事务排序，细节我没有理解得很清楚，不过据说它的限制很强，不能支持 begin ... commit 这种交互性事务了，于是在 SQL 语义这边是玩不转的。作者[对 2PC 的批评](https://dbmsmusings.blogspot.com/2019/01/its-time-to-move-on-from-two-phase.html)很透彻，我们倒是可以看一看他是怎么说的。两阶段提交中，会有参与者(workers)和协调者(coordinator)，如果 work 投了 yes，然后它挂了怎么办？因此必须涉及到将自己的投票持久化的(这是一次磁盘io的来源)。

两阶段提交有两个问题，第一个叫 "blocking problem"，假设所有的 worker 都投了 yes，但是 coordinator 挂了。注意，coordinator 是具有绝对权力的，而 worker 一旦投票了，是没有机会再修改的，于是这时就会出现系统卡住的状态，除非 coordinator 恢复，否则无法继续推进。

第二个问题叫 "cloggage problem"，这个问题不像前一类那么广为人知。两阶段提交是在事务处理中，**事务的延迟会上升，因为要经历 prewrite 和 commit 两个阶段。而这期间事务的最终状态并不可知，因此它是需要阻止其它事务的，只有当 2PC 完成之后，被这个事务两阶段提交阻止的事务才能继续推进。这会进一步地增加延迟，降低吞吐**。

所以 calvin 给出的解法就是不要 2PC，而是在前期做事务的冲突检测，处理完之后不会发生冲突，直接到 log 层。

## foundation db

推演到这一步，我觉得还是很对我的味口的，以 log 为中心，然后把 2PC 和 raft 那一坨东西，都变成前期的事务冲突检测。

从某种角度来看，tidb 也是以 log 为中心，只不过它的实现路径用 2PC + raft 来完成事务的冲突检测的。Aurora 也是 log 为中心，但是它是1写多读的，实际上绕开了分布式事务那一层，但缺点是写入性能不能水平扩展了。

在查资料的过程中，我读到另一个有趣设计，它就是 foundation db。非常巧合，这个设计几乎跟我推导得到的架构一模一样。log 之后都没有什么好说的，它是把事务跟 log 解耦开了的。关键点就是原文里面的 resolver 那一层，是一个事务冲突处理层。

我们假设客户端向两个不同的实例提交分布式事务，事务的修改涉及重叠的 key range，如何进行冲突检测呢？只要把两个实例上面的事务的修改集，都发送到另外的一个节点上面，该节点就拥有全部冲突检查需要的信息了。在 foundation db 里面这个就是 resolver，前面不同的 proxy 层会把事务修改集发送到 resolver 层进行集中的冲突检查，然后确认事务没冲突后就可以再发送到 log server 以及进一步到 storage server。

我们来计算一下这种模式下的写入路径的开销。

- 事务定序跟 tidb 那边需要取两次 tso 是一样的，两次网络。
- 数据发送到 resolver 进行事务冲突检测，需要一次网络。
- 在 resolver 中的冲突检测是全内存的，不涉及磁盘。这比走 2PC+raft 写盘和网络要快得多，而且没有 cloggage problem
- 从 reslover 到 log server，一次网络
- log server 的持久化，一次磁盘

到了 log server 之后的事情都是可以异步的，也就是说 log 记录成功后就可以返回给客户端了，后面的事情不影响写入延迟。所以 foundation db 这边总共是 4 次网络，1 次磁盘。

既然聊到了 log 完成后就算完成，而不是 storage server 完成算完成，那么就不得不再提一下优化的概念。tidb 那边 6 次网络，4 次磁盘，是按完全没有优化的情况下计算的。之前我写过[TiDB 的事务模块演进](tidb-transaction-evoluation.md) 在 TiDB 5.0 里面有一个很重要的优化，叫 async commit，在这个优化之后，2PC 里面可以不用等待 prewrite + commit 完成，而是在 prewrite 完成后，直接回答客户端写入完成，然后异步去执行 commit。这样直接节省了两次网络两次磁盘，于是就是 4 次网络，2 次磁盘了，比 foundation db 已经没有太本质的落后了。然后如果 1PC 或者优化掉 commit ts，则进一步节省一次网络，变成 3 次网络 2 次磁盘。

这么想好像 tidb 当前的架构设计也还是挺牛X 的，相比 foundation db 这种确定性事务，差异只在把事务冲突检查放在预告处理层还是 2PC 层的区别？其实，应该说在无冲突场景下，平均延迟或者 P80 可能两种不同设计并不会导致特别大的写入延迟上的差距。但是在冲突场景下，foundation db 的模型一定更优，为什么？因为它冲突检查是全内存操作，失败的代价是更小的。而 tidb 这边在发生冲突的情况下，能检查到冲突的时期更晚，数据写入和 raft 开销是沉没成本，所以冲突的代价更高。当然，这里还涉及其它因素，乐观悲观事务之类的，准确对比只能说大家都是乐观事务的场景l下的对比。冲突延迟越早做，越是在内存做，发生冲突后的处理代价就越低，长尾延迟就更低。

## 继续细化场景

foundation db 的架构好像已经是最优的了。但是我们可以换一个角度再来思考一下。

单机数据库和分布式数据库，单机的性能更优。因为分布式一定涉及分片，涉及分片就导致需要 2PC，也就是导致性能问题的关键。
所以我们在一个角度是说，能不能干掉 2PC，从而得到更好的性能。也就是像 foundation db 那样把事务冲突处理放到前面一层。
换成另一个角度是，能不能没有分片，于是从需求侧干掉 2PC，从而更好的性能。

实际的用户场景中，很多都是数据库表并没有那么大，并不需要用上分布式数据库，比如单机 mysql 也能够胜任。只不过国产化替代这种场景，那么对应的需求就是库比较多，减少运维成本，比如说一套部署就能全搞定。
如果纯粹这种场景，可能 Aurora 的架构更优。不过考虑国内情况，都不愿意把数据放到云上，而是需要私有化部署。

这些细化的场景也是一些架构适配的可能性，只要数据量不大，就可以不用分片，就可以有一些其它的设计空间。最好的情况，是即能支持分布式，又能像集中式那边对小库小表消除分布式的开销，性能好，管理和运维的成本还低...细节还没想明白，反正烧死了不少脑细胞了，就先思考和记录到这里吧。
