[这篇文章](https://www.cockroachlabs.com/blog/transaction-pipelining/)很有意思，简单写几句。

早期 cockroachdb 在 begin ... commit 之间，每执行一条 SQL 语句都是把 write intents 完全写到存储之后，才返回给客户端。这样导致一个问题，整个的事务执行时间是跟 statement 数量正相关。performance 上不去，为此他们进行了优化。

他们曾经评估过的一种方案是，所有事务的所有写入先缓存在本地内存，直到 commit 的时候，才真正去执行提交。这正好是 tidb 使用的模型，然而他们并没有采用该方案。理由有两点，一个是他们发现这个方案，事务会持有写锁产生大量读写冲突，导致读操作失败。尤其是在一些 workload 比如 TPCC 下面，影响很大。另一个理由是，读写使用不同的 timestamp 导致隔离级别会弱一些，比如 snapshot isolation。

对于冲突问题，我的评价是：fair and objective，哈哈。继续往下说，他们没有采用这套方案，那他们是怎么改进的呢？

他们把事务里面执行里面 statements 的过程流水线化了。即先返回结果，同时异步去写 write intents。只要保证最后 commit 的时候，所有 statement 的异步 write intents 都是完成了的，正确性就没问题。

异步写 write intents，这个时候没执行结果呢，拿什么返回给客户端？他们观察到，其实写操作返回的结果只需要成功失败，以及影响了多少行。而在 range 的 lease holder 中拥有所有必需的信息。

于是可以不做复制就返回结果。相当于在内存里面操作完，就把结果返回去。之后异步地同步状态。分布式共识说白了就是状态同步过程：改状态，状态通过复制，达成共识，之后返回客户端。lease holder 说自己状态是多少，然后把这个状态同步到其它 peer 上面。

这当然是可能失败的。比如 lease holder 切换了，各种故障的场景等。于是流水线之后会出现一种情况，statement 返回客户端操作成功，实际操作是失败的。直到 commit 会抛出错误来。他们很鸡贼地利用了，只有 commit 的成功才决定整个事务的成功。就好比 CPU 的指令流水线，先预取，预测执行，失败后无非将结果丢弃重来，打破流水线，而预测成功则可以带来巨大的性能提升。

做完这个优化之后会造成一个问题，会跟 MySQL 一些基于锁的行为，语义上不太一致。有些业务程序或者框架就假定使用 MySQL 的行为，会无法兼容。
