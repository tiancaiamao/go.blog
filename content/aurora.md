Amazon Aurora: Design Considerations for High Throughput Cloud-Native Relational Databases

## Abstract

aurora 是一个 OLTP 的关系型数据库。这篇 paper 里面写了架构和设计时的考量。高吞吐的数据处理瓶颈，已经从计算和存储，转移到了网络。这是 paper 一个重要观点。

> We believe the central constraint in high throughput data processing has moved from compute and storage to the network.

aurora 提出了一个新颖的架构，将 redo processing 推到一个多租户，scale-out 的存储服务里面。这样做不仅节省了网络带宽，而且可以快速故障恢复。

划几个重点

* 它目标其实是解决多租户 scale-out 问题
* 认为网络是瓶颈，这个架构可以减少网络的代价
* share storage，出故障了直接从副本拉起来，上层无状态
* log as database

## 1. Introduction

IT 基础设施现在移到云上去了。现代的分布式云服务，通过计算存储分离，通过存储 replicate 到多个节点上面，以获得弹性和扩展性。

> In modern distributed cloud services, resilience and scalability are increasingly achieved by decoupling compute from storage and by replicating storage across multiple nodes.

在多机群的环境下，IO 要跨很多节点和很多 disk，导致传统数据库在这种场景下会遇到 IO 瓶颈（它这里指的应该是在云服务上面的虚拟存储）。

database 操作之间，需要同步，导致 stall 和上下文切换。然后会有缓存的伪共享(false sharing)问题。

> In this paper, we describe Amazon Aurora, a new database service that addresses the above issues by more aggressively leveraging the redo log across a highly-distributed cloud environment.

redo log 分离是 aurora 架构设计的关键改动。下层是一个虚拟分片的 redo log 存储服务，上层是多租户 database 实例，两者之间松散耦合。

每个 instance 保留传统数据库的 kernel 部分，包括 query processor，transactions，locking，buffer cache，access 和 undo 管理。挪到存储服务里面的是 redo logging，持久化存储，crash recovery，以及备份和恢复。

相对传统数据库的优势：

1. 存储层是独立的能容错且自愈的服务，并且跟上层是分离的，因此数据库不会受到相关问题影响，下层对上层屏蔽了这类错误
2. 只写 redo log，可以将网络 IO 减少一个数量级
3. 将备份和 redo 恢复等功能变成了持续的异步写整个分布式存储(这些在传统数据库里面至关重要，往往是同步且 expensive 的)

> Second, by only writing redo log records to storage, we are able to reduce network IOPS by an order of magnitude. 

## 2. DURABILITY AT SCALE

数据写进去了，必须是持久化的。

### 2.1 Replication and Correlated Failures

instance 可以随时添加减少，而 storage 其实不应该随 instance 的生命期动态变化。否则，storage 的 fail 就必须通过副本做容错。在一个系统里面，每一层都有自己的错误风险率，节点，磁盘，网络等等。分离存储是让风险降低的。

用 quorum 做，要保证一致性，必须满足 `R+W > N` 。这样读到的数据中一定会包含最新的版本。要避免写冲突，写必须写入超过半数 `W > N/2`。常见是 3 副本，写 2 份读 2 份。

论文里面认为，3 副本不够。跟 amazon 内部的AZ (availability zone) 配置环境相关。他们是 6 副本的，写 4 份读 3 份。

### 2.2 Segmented Storage

> We instead focus on reducing MTTR to shrink the window of vulnerability to a double fault.

关注的是平均修复时间 MTTR(Mean Time to Repair)。为了获得较低的 平均修复时间，将 database volume 切成小的固定大小的 segment，当前是 10G 每个 segment。6 副本形成一个 PG(Protection Group)，所以每个 PG 是 6 个 10G 的 segment。分散到 3 个 AZ　里面，每个 AZ 2 个 segment。storage volume 是一系列 PG 串起来，物理上丢到存储节点集群中。PG 是随着 volume 的增长而分配，目前支持到 64TB。

segment 是独立的失败和修复的单元。10G 的 segment 可以在 10s 恢复。

> A 10GB segment can be repaired in 10 seconds on a 10Gbps network link. 

只有在同一个 10s 窗口内发生了两次故障，并且再外加一次 AZ 故障才会导致 quorum 数量不够。这个发生率是极其低的。

划重点，这一小节主要内容是说，将数据分割成小块，每个小块又有副本，同时恢复速度极快，以此来达到容错。

### 2.3 Operational Advantages of Resilience

只要系统的容错可以容忍长时的故障，那么它也是可以容忍更短时的故障的。aurora 的容错能力特别强，利用它来做一些维护的事情，可以简化问题。
比如说热点管理，直接把热点的 segment 标记为 bad，然后快速修复就可以自动迁移到其它节点上面。甚至连操作系统打补丁，软件升级之类的，都可以这么干。滚动升级，一次处理一个 AZ。


## 3 THE LOG IS THE DATABASE

这一节解释为什么传统数据库，在分片 + 副本 的存储系统里面，会造成网络 IO 问题以及同步写阻塞。然后解释 aurora 为什么把 log 拿出去做进存储服务里面，以及这么做为什么能极大的减少网络 IO。

理解这一块，也就能理解他们为啥要开发 aurora 了。

### 3.1 The Burden of Amplified Writes

segment 6 副本，并且要写 4/6 份，可以获得极强的容错弹性。但是如果像传统数据库那么搞，每次写操作会生成非常多的 IO，放大特别严重。
并且 IO 的同步会造成 stall 以及高延迟。chain 的方式 replication 可以减少网络开销，但是同步的 stall 和 latency 会更高。

传统数据库，同时要写数据页 (btree等)，同时还要写 redo log（WAL）。redo log 记录 page 在修改前后的差别，在 page 的 before-image 上重放 log，就可以得到 after-image。

为了高可用，还会有主从同步，主从同步又会把整个开销放大一倍。论文这里有张图特别形象，写 log，写 binlog，写 data，还要写元数据文件。
在 EBS 还有写 EBS mirror。

写的步骤中好些还是要串行，同步写的，延迟很高。有很多不同类型的写入，他们其实是把重复的信息做了多遍。

### 3.2 Offloading Redo Processing to Storage

传统数据库里面，是先写 redo log，再将 log apply 到内存中的 page。事务提交 log 必须是写入的，不过数据页的修改可以推后。
在 aurora 中，跨网络唯一要写的只有 redo log。不会写 data 页等其它东西。log 会推到存储层，后台可以用 log 生成 database page。

> In Aurora, the only writes that cross the network are redo log records. 

如果要等页生成完才返回，这个是比较耗时的。因此，是在后台持续地做物化(materialize)。后台物化过程只是可选的... 其实 the log is the database ... 存储层的物化过程，只是把 log 应用后生成一层 cache。

物化跟 checkpoint 不同的是，checkpoint 是针对所有的 log chain，而 aurora 的 page 物化只针对该 page。

> Checkpointing is governed by the length of the entire redo log chain. Aurora page materialization is governe by the length of the chain for a given page.

(这里有一个疑问，如果只针对 page，不同 page 之间 log apply 到的位置不一样，那么它如何保证当前 snapshot 的全局一致性？)

相比于 mysql 模式搭主从，在 aurora 模式下面只有主的 log 写入到存储服务，只要写够了副本数，比如 4/6 就算是 durable 了。从就可以直接用 redo log 去 apply 并更新缓存。

在 30 分钟的测试表明，mysql 的写放大是 aurora 的 6 倍以上。这个时间内完成的 transaction 是 mysql 的 35 倍。

将存储分离之后，将 checkpoint，data page 写入和备份等等后台处理工作都一并干掉了。crash recovery 时间缩短，可用性提高。

>  In Aurora, durable redo record application happens at the storage tier, continuously, asynchronously, and distributed acros the fleet.

### 3.3 Storage Service Design Points

存储服务的设计重心之一，是尽量减少前端写请求的延迟。大部分存储处理都是在 background 的。

trade CPU for disk。存储节点如果前台的请求比较忙的时候，就不要 GC 了，除非是磁盘容量快满。当前台处理越重时，后台处理就减少，这跟传统数据库里面是不一样的。

存储节点里面的各种活动，涉及以下步骤：

1. 接收 log 记录，加到内存队列
2. 持久化 disk 并且 ack
3. 组织 record 并判断 log 的 gap 有多久
4. gossip 协议让 peers 去 fill gap 
5. 将 log record 合并成新的 data page
6. 周期性地将 log 和新 pages 存到 S3
7. 周期性地 GC 旧版本
8. 周期性地校验 pages 的 CRC

只有 1 和 2 会影响 latency

## 4 THE LOG MARCHES FORWARD

这一节讲如何从 database engine 生成 log，使得持久化状态，运行时状态，以及副本都是一致的。并且如何不通过 2PC 来实现。

### 4.1 Solution sketch: Asynchronous Processing

每条 log record 都关联了一个逻辑序列号 Log Sequence Number(LSN)，单调递增。维护 consistency 和 durability 两个位置，并且在收到 ack 之后会持续地将这两个位置往前推进。由于有些节点可能会 miss 一个或者多个 log record，所以它们之间需要 gossip 交换信息，并且补齐 gap。只要不发生 recovery 异常，直接可以根据 runtime state 读单节点，而不需要读 quorum。

>  The runtime state maintained by the database lets us use single segment reads rather than quorum reads except on recovery when the state is lost and has to be rebuilt.

(疑问，这里的一致性是最终一致性么？ 各个 instance 上面的 runtime state 并不是同步的呀！)

数据库可能会有多个进行中的相互 isolated 的事务，最后的完成顺序可能跟初始顺序不一样。假设数据库 crash 了或者 reboot 了，各个事务是否 rollback 是相互独立的。track 哪些事务完成了一部分，需要 undo 的逻辑，还是留在数据库引擎里面，就像是写盘一样。但是重启时，在数据库能提供服务之前，存储服务需要确保，恢复不是按 user-level 事务，而是保证提供一个 uniform view。

存储服务需要确定最高的 LSN，在它之前的 log 都是可用的。这个最高的 LSN 叫 Volume Complete LSN(VCL)。恢复的时候，比 VCL 更大的 LSN 都丢弃。

> During storage recovery, every log record with an LSN larger than the VCL must be truncated.

(这里肯定有一致性相关的细节没展开)

* VCL (Volume Complete LSN): 由存储服务决定的最大的 LSN，在 VCL 之前的 log record 都是可用的。
* CPL (Consistency Point LSN)： 由 database 给 log record 打标签决定
* VDL (Volume Durable LSN)： CPL 中最小的，小于等于 VCL。

这几个概念无非就是 log 同步到哪个位置了，以及 log apply 到哪个位置了，理解并不难。

database 与存储之间的交互如下：

1. 每个 database-level 事务，会拆分成多个 mini-transaction(MTRs)，必须有序并且是原子的
2. 每个 mini-transaction 由多段连续的 log records 构成
3. mini-transaction 中的最后的 log records 是一个 CPL

(这个地方是切分要保证不会有事务成功一半)

recovery 的时候，database 会问 storage service 确定每个 PG 的 duration point。并且会用它确定 VDL，大于 VDL 的 log record 需要 truncate 掉。

(也就是事务写入一半时没有回滚，在这里会把多写入的给 truncate 掉)

> On recovery, the database talks to the storage service to establish the durable point of each PG and uses that to establish the VDL and then issue commands to truncate the log records above VDL.

### 4.2 Normal Operation

#### 4.2.1 Writes

> In Aurora, the database continuously interacts with the storage service and maintains state to establish quorum, advance volume durability, and register transactions as committed. 

database 会持久地跟 storage service 交互，维持状态确定 quorum，往前推进 durability 位置，并将之标记为 committed。

database 会为 log record 分配唯一，有序的 LSN，新分配的 LSN 大于当前的 VDL，并小于最大 LSN 限制 LSN Allocation Limit (LAL)。

注意每个 PG 的每个 segment 里面只有一个 log records 子集，只影响 segment 里面的 pages。每个 log record 里面包含了一个后向链接，识别该 PG 的前一个 log record。这个 backlink 可以确认 segment 的完备性，作为写满一个 segment 的标记。存储节点会 gossip 会用到这个信息，来确定缺失的 log records。

#### 4.2.2 Commits

在 aurora 里面，事务 commit 完成是异步的。收到客户端的 commit 后，处理线程把相应的 commit LSN 记录下来，等 VDL 大于等于这个位置的时候，就是完成了。worker 线程并不会同步地等等 commit 完成，而是继续处理其它的。


#### 4.2.3 Reads

读的时候有 page 缓存，只有当缓存不中，才会到 storage IO 请求。buffer 满的时候就要替换旧的。在传统数据库里面，如果 page 是 dirty 的，就需要刷到磁盘里面。而在 aurora 里面不是在刷新 dirty page 的时候写入，它强制要求 buffer cache 必须一直是最新的。 这个很简单，就是保证 page LSN 是大于等于 VDL 的。

正常情况下，database 并不需要保证读 quorum。读的时候要确认一个 read-point，代表请求发起的时候的 VDL。(说白了，这里就是历史读)

> When reading a page from disk, the database establishes a read-point, representing the VDL at the time the request was issued. 

注意这里读的时候有 MTR 切割，事务必须看到的是完整的。

database 跟底下 storage node 交互过程中，它是知道哪些 segment 中可以读到它要的数据的(read-point 位置跟 SCL 对比)。

database 能知道哪些读操作当前正在执行中，所以它可以对每个 PG 计算最小的 read-point。相互之前 gossip 之后，就可以知道一个整体最小的 read-point。那么 GC 就可以用这个信息做物化。

并发控制跟传统数据库里面是一样的。

#### 4.2.4 Replicas

在 aurora 中，可以单个 writer，15 个 read replicas mount 到单个共享存储上面。加读副本是没有额外开销的，既不会增加存储量，也不会增加磁盘写入。

为了减少延迟，writer 生成的 log stream 除了发到存储节点，还会发给 read replicas。

replica 在应用这些 log 的时候要注意，log records 的 LSN 必须大于等于 VDL 才能 apply；注意一个事务产生的多条 log 在应用时的原子性。

### 4.3 Recovery

传统数据库里面，用 checkpoint + WAL 来做 recovery。page buffer 是只是 cache，要么多数据，要么差数据。必须在 checkpoint 基础上重放 WAL 才得到正确的 page。多做 checkpoint 可以缩短恢复过程，但是做 checkpoint 又会对前台业务产生影响。

在 aurora 里面，redo log 的 apply 是丢到了存储节点里面，并且是在后台时刻运行。database 恢复的时候，就是直接从存储服务上面恢复，速度非常快。

database 刚起来恢复运行时状态的时候，要读 quorum，这样来满足一致性。计算 VDL，并且 truncate 掉做到一半的事务。

> The database still needs to perform undo recovery to unwind the operations of in-flight transactions at the time of the crash. 

database 可以在 online 之后，再去 undo recovery.

## 5 PUTTING IT ALL TOGETHER

这里给了一个整个的图。图里面非常清楚了，这一节废话比较多。

aurora 是在社区版 mysql 上面改的。先用了一段介绍了一下 mysql innodb 的执行流程。可以略过。

在 aurora 中，redo log records 组织成 batches，batches 会按每个 log record 所属的 PGs 进行 shard。 redo log records 表示的是每个 MTR 中必须被原子执行的修改。读副本从 writer 获取事务的 start 和 commit 信息，并用这些信息支持快照隔离。并发控制是完全在 database engine 实现的，不影响存储服务。存储服务提供了一个统一的视图，就跟从 innodb 里面读数据一模一样。

每个集群的 database instance 包括一个 writer，零到多个 reader。

## 6 PERFORMANCE RESULTS

sysbench 测试，使用的实例越牛X 性能就越牛X。

在 database size 为 100GB 时，写入的 tps 是 mysql 的 67 倍。

当数据库大小增加时， aurora 写入性能降低了...这个为啥？

## 7 LESSONS LEARNED

写了一些用户经验的东西，或者说叫客户需求的东西。

## 9 CONCLUSION

>  The big idea was to move away from the monolithic architecture of traditional databases and decouple storage from compute. 

存储计算分离。

> With all I/Os written over the network, our fundamental constraint is now the network.

网络是瓶颈。

使用 quorum 模型做容错，log processing 降低 IO 负担， asynchronous consensus 消除多阶段的同步协议的开销，离线的 crash recovery，checkpoint 做在分布式存储里面。

最终，这个方法简单了架构，降低复杂性，scale 很好。
