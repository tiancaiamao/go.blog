公司内部在做 TiDB Internal 系列培训，弄完 PPT 发现，写篇博客也挺有意思。就事务这个话题讲一讲。

## 第一部分 MVCC

为什么要基于 MVCC 呢？因为 MVCC 的粒度比锁要好。普通的锁是全部操作都阻塞的；读写锁中，读不阻塞读，不过读会阻塞写；有了 MVCC 之后，大家写不同的版本，相互之间不干扰。读旧版本，不会阻塞写入，这就比读写锁的冲突粒度更低了。

然后说说 CAS。CAS 这个东西，就是一个原子操作。即使并发执行，也只会有其中一个成功。比如 `CAS(v0 -> v1)`  `CAS(v0 -> v2)` 同时执行，最后的结果要么是 v1 要么是 v2。

那么 MVCC + CAS 一起使用，会有什么启发呢？ 我们可以发现事务的本质！普通的 CAS 概念只会改一个 variable，而事务会涉及许多个修改。一旦我们把 MVCC 概念也加进来，所有修改之前的值 MVCC 版本叫 v0，修改之后叫 v1。那么事务就是 CAS(v0 -> v1)。

TiDB 事务模型是按 percolator 实现的。事务启动要拿一个 start ts，等到提交要拿一个 commit ts。其实分别对应着 v0 和 v1。也就是用 start ts 拿到一个 snapshot，这个就是 MVCC 的版本 v0。然后在 snapshot 的基础上修改，修改之后其实是 MVCC 的版本 v1，
用 commit ts 提交时，做的事情就是 CAS(v0 -> v1)。确认 start ts 到 commit ts 之间，没有事务冲突，否则这个 CAS 就会失败了。

    Get snapshot at start ts
    Check no modification at commit ts
    Reject the operation if CAS fail (transaction conflict)

所以我们可以得出：**这个事务模型的本质，其实就是 MVCC + CAS**

接下来，是如何实现这个 `CAS(v0 Snapshot  => in-memory modification => v1)` 操作。也就是一个事务在 start ts 和 commit ts 之间，没有同时被其它事务修改过。这个理解很容易，在 start ts 和 commit ts 之前划一条线，没有 overlap 就可以了：

这种是不行的：

    T1 start ts  ----------------------------->    commit ts
    T2              start ts ----> commit ts
    
同样，这种也是不行的：

    T1 start ts --------------------> commit ts
    T2              start ts -------------------------> commit ts

都有重叠了。

实现这个检测，就是在写操作的时候，对数据上锁。所谓数据上锁，就是在数据的值上加一个特殊的标记就行了，有这个标记，表示有人正在写，锁住了这个值。读操作是不给数据上锁的，因为读不会发生在数据上加标记这个动作（否则读就要写东西了）。

* 写写冲突：后面的写操作，会发现前面写操作留下的锁
* 写读冲突：后面的读操作，会发现前面写操作留下的锁
* 读读操作：读读不冲突，没锁也没关系
* 读写冲突：这是最麻烦的。因为读的人不会在数据上加锁，所以写的人不知道有人在读

看这个时序：

    T1 start ts ----------> t1 write ----> t1 commit
    T2          start ts ------->

T2 在 start ts 的时候读，读到 T1 开始之前的值（T1 write 还没执行，即使执行了， T1 的改动也还在本地内存未提交）

T1 在 write 的时候，不知道 T2 有读过（读不会上锁，检查不到）

这种读写冲突是在 commit 的时候去检查：

    T1 start ts --------------------> commit ts
    T2              start ts -------------------------> commit ts

T2 在 commit 的时候，它会发现 T1 已经 commit 过了（数据版本变了），所以它会 abort 掉。

最后一个问题，是 tidb 事务是乐观锁机制。所以 start ts 到 commit ts 之间发生的 modification，都是记录在内存，直到提交才写到 tikv。那么 lazy commit 的正确性呢？ 在 commit 时检查也仍然是没问题的。

## 第二部分 2PC

为什么需要 2PC 呢？ 为了保证原子性。前面说到了，TiDB 事务本质是是一个 MVCC + CAS。到了 CAS 这一步（其实也就是事务 commit），TiDB 一个事务的改动，是涉及到多个 region (raft group) 数据的。
假设修改其中一个 region 成功了，修改另一个 region 失败了，那就麻烦了。所以必须保证多个 region 同时能成功，原子性破坏，所以需要 2PC。

2PC 的第一个阶段是 Prewrite 阶段，预先把要改的数据，写到各个 region 上面。每个 region 就是 participant 的概念。如果所有的 region 写入都能成功，那么就进入到第二阶段，Commit 阶段。

Prewrite 已经把数据都写好了，但是有一个标签，说这些数据还不是可见的，走到 Commit 阶段就是说，所有 participant 选票都 OK， 那么提交吧。提交也就是把数据标签变成可见。

但是这里同样有一个问题：把所有标签变成可见这一步，也需要是原子的步骤。怎么实现？这时我们就有了 primary key 的概念。每个事务，指定一个 primary key 作为事务的代表。如果 primary key 是提交成功的，事务就成功。如果 primary key 失败了，事务就失败。

primary key 只有一个 key，所以只落在一个 region 上面，不会出现一半成功一半失败的问题，这个是由 raft 来保证的。

每个 region 是多副本的，这里其实也涉及到副本间的一致问题，raft 也是一个很大的话题，就不在这里单独展开了。总结一下，就是

* 2PC 保证一个事务分散在多个 region 上的数据写入原子性，对事务选取一个 primary key，primary key 不会涉及多 region 问题
* raft 保证同一个 region 上面多副本的一致性

注意，percolator 里面是没有 coordinator 的。这里会涉及到 primay key 提交成功了，事务的其它修改还没成功，或者是 primary key 提交失败了，需要清理的工作一类。

## 第三部分 TSO

略

[PPT link](https://docs.google.com/presentation/d/19ARcGdA0majRipJvEPfaES8eUyM-lunS_mPwipic8e4/edit?usp=sharing)
