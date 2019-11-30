## 我们为什么需要大事务

用户是有大事务的需求的

开启 batch dml 以后，无法保证事务的完整性

## 为什么当前不支持大事务

* tikv 扛不住?

    titan 对单个大的 key-value 的支持能力已经大大增强
    prewrite 是按 region 的，单个 region 并没多大
    我们可以先假设底层引擎是扛得住的

* TTL 太小事务被干掉

    事务的 primary lock 里面记录了事务状态，有一个 TTL 用于异常处理
    TTL 的值太小，在 prewrite 过程中，事务就会被清锁干掉
    TTL 设置的太大，会阻塞其它事务
    如果事务异常挂掉，锁会很久清不掉，会一直 block 导致其它事务超时失败
    解决方案：自动更新 TTL

* 事务的阻塞

    写写冲突，没法解决
    读写冲突，**只要让大事务不阻塞读操作，就可以接受**
    解决方案：新的事务模型

* 事务冲突代价太高

    不在解决范围之内
    或者可以考虑悲观事务

## TTL 自动更新

初始设置一个比较短的事务 TTL，然后在事务存活期间，自动更新 primary lock 的 TTL 值

这样就可以解决大事务的 TTL 问题

* TTL 自动更新导致 ResolveLock 需要调整
* TTL 自动更新需要引入死锁检测

### 死锁检测

乐观锁里面不需要死锁检测，因为 TTL 不会更新，即使发生了死锁，过了 TTL 锁也能清掉

自动更新 TTL 之后，需要引入死锁检测

## ResolveLock

以前检查 secondary lock 跟检查 primary lock 都一样

操作步骤：

* 检查到 secondary(primary) lock
* backoff TTL 那么久
* 调用 CleanUp 将 primary lock 给 Rollback
* 调用 ResolveLock 清理 secondary lock

现在 secondary lock 里面信息无效，只有 primary lock 里面是真实的

* 检查到 secondary(primary) lock
* 去检查 primary lock 判定事务状态 (CheckTxnStatus)
* 如果事务 TTL 过期，则清锁
* 如果事务 TTL 没过期，则 backoff 之后重新检查

### CheckTxnStatus 两种异常场景

遇到 secondary lock 了，再去检查 primary lock，发现 primary lock 不存在。

什么情况下会遇到？

* 由于 prewrite region 并发，secondary lock 先写成功，primary lock 还没写
* 由于悲观锁回滚不留下 tombstone

## 非阻塞读

### 为什么遇锁会阻塞读操作

一个事务正在写入，另一个事务去读取。能不能读正在写的内容？

* 读了，事务回滚了 -- read uncommitted
* 不读，事务成功了 -- lost update

读不读都有问题，所以要：等。

等写操作执行完了再读，就没这个麻烦了。

### 事务顺序

从第二个点想想办法，我们讨论不读

如果写事务在前面，读事务在后面，不读就会出现写丢失。

所以 **关键点是重排序，让读事务在前面，写事务到后面**，这样读事务就可以不读到写事务

如何调整事务顺序呢？ 事务顺序是由 ts 决定的。

*一个事务1的 start ts 大于另一个事务2的 commit ts，那么事务1是在事务2 后面的*

假设每个 ts 都唯一，并且不考虑 +-1 问题，我们可以换一种说法：

*一个事务1的 start ts 小于另一个事务2的 commit ts，那么事务1是在事务2 前面的*

所以让读事务，把写事务的 commit ts 往后推，保证读事务 start ts 小于写事务 commit ts

即可达成顺序调整，从而实现重排序

### min commit ts

* 在 primary lock 里面，记录一个 minCommitTS 字段

* 每次读者遇到锁的时候，调整 minCommitTS，使它大于 reader ts

* commit 的时候，最终 commit ts 要至少大于 minCommitTS

即：**每次读事务，遇到写事务，读事务会把写事务推到自己后面**


## 测试中遇到哪些问题

### 并发压力过大，tikv 被打死了

prewrite 的时候，按 region 数量并发

大事务需要控制并发量，否则 tikv 会被打死

### insert 执行慢

coprocessor 是并行读的

insert 的时候是串行的

在 insert 那里会形成一个瓶颈点，大事务比较明显

### 内存问题

memdb 是大块连续内存，在大事务下会分配失败 => 改造成分块的，每块大小上限 128M

insert 过程中，table 接口是用的 []Datum 表示，非常占内存 => 改成尽快刷到 memdb 里面

2pc 里面 mutation 的内存占用高

纯 KV 数据量 2-3G 的大事务，在 mutation 里面的表示大概 7-8G

加上其它包括 dirty table 和进程本身之类会到 10G

由于 Go 的 GC 策略，向系统申请的内存量会在 20G

大概 6 倍的放大

预计支持 10G 事务需要 60G+ 的内存

2-3G 的事务完成时间大概是 10min
