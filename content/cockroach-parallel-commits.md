## 在 raft 之上构建分布式事务

cockroachdb 的[一篇博客](https://www.cockroachlabs.com/blog/parallel-commits/)讲了他们新实现的的并行提交。这一篇我不得不由衷的赞美，应该是本年度读到的最佳的一篇内容吧。它描述了一个在 raft 之上构建事务的方法，并且克服了 2PC 在 prewrite 之后 coordinator 的等待问题。这个实现方式，揭示了 2PC 和 raft 的一些本质的关联性。

## 两阶段提交的问题

把两阶段提交的问题写得非常透彻的，当属于[这一篇文章](http://dbmsmusings.blogspot.com/2019/01/its-time-to-move-on-from-two-phase.html)。

> The well-known problem is referred to as the “blocking problem”. 

如果投票阶段每个参与者都投了 yes，但是协调者挂了，没有把最后的 commit 通知给参与者，两阶段提交就没法完成。挂了之后的恢复也是一个头痛的事情。最核心的点还是在于，coordinator 拥有绝对的权力，整个协议都会 block 在这里。

> The lesser-known problem is what I call the “cloggage problem”. 

在 2PC 之上构建事务，事务的延迟就会是受到 2PC 的协议的（阻塞的）影响。事务本身的冲突会导致在两阶段提交的中途，失败。事务之间会阻塞延长 2PC 过程，而 2PC 过程的延长反过来又会增加事务冲突机率。也就是相互的影响的。其实第二个问题，就是直接在说 spanner 模型的缺点了，模型不改的话，这个问题是固有的。

我主要关注的还是前一个点，**在 2PC 之上构建事务，prewrite 等待所有参与者完成之前，不能进入 commit 阶段，这是一个非常严重的 block**。cockroachdb 的博客里面写的优化，就是要解决掉这个问题。

## 事务的可见性

回忆一下在 2PC 之上构建事务是怎么处理的，选出单个 key 决定事务状态。当这个 key 的状态由 Lock 变成 Commit，事务就对外可见了，由此来保证事务变更是原子的。

然后由 raft 来保证单个 key 的状态变更的一致性。prewrite 阶段要确认所有参与的分布式结点都成功，成功之后是不会再失败的。commit 阶段把事务的状态的 key 提交成功后，事务就对外可见了。

这里面的问题是，我们有两层协议。一层是事务那边做两阶段提交，一层是 raft 那边用来保证单 key 的一致性。两层协议造成的问题是延迟上升。2PC 那边 Prewrite 就要 block 一次，然后操作要走 raft 又得同步多副本，这样子延迟就上去了。

有没有办法把这两件事情合成一个呢，直接在 raft 之上构建事务呢？ 这时我们会发现，一点有趣的事情。

## raft 和 2PC 的共性

**2PC 和 prewrite 和 raft 的 log replication，其实是同一件事情**。2PC 的 prewrite 要求参与者投了 yes 数据就不会丢了。raft 的 log replication 是同步到多数结点后，数据就不会丢了。它们共同点都是持久化。

有了这个发现之后，我们可以用 raft 的 log replication 成功，来等价替换 2PC 的 prewrite 阶段。举个例子，

begin t; write1; write2 ... commit

我们可以直接 raft 的 replication 写完 write1 write2，就表示 prewrite 成功了。commit，原本是要等 write1 write2 都成功了，才能够发送的，也就相当于在 2PC 里面的 commit 要等 prewrite 所有参与者投票成功了才能做。

接下来是优化掉 2PC 的 prewrite 阶段等待，不等前面 write1 write2 返回，先并行的直接执行 commit 操作。

cockroachdb 的优化点是在这里，它可以让 commit 不等所有 write1 write2 成功。也就是 commit 不等全部的 prewrite 成功，就开始执行 commit。
它新引入了一个 STAGING 阶段，在这个阶段下，在 begin ... commit 的最后收到 commit 命令后，发送 commit 消息不等前面的操作成功。这个 commit 会将事务状态 key 标记为 STATING 状态。
在 STAGING 状态下，能确认前面的 write1 write2 操作的 log replication 完成了，就可以算事务成功了，能返回客户端结果了。

新掉入的这个 STAGING 状态，就直接消除了 2PC 里面， commit 要等待 prewrite 全部成功的问题。减少了一轮的 RPC。

细节可以看一下原博客，不过多说一句，失败后的处理流程，其实还是很 tricky 的。

## calvin 和 spanner

在年初的时候写过一篇 [calvin 的 paper reading](/calvin.md)。calvin 和 spanner 是两个不同的流派。前者来自于学术业，而后者来自工业界， google 搞的。后来 calvin 发展成了 FaunaDB，学术成果工程化的产物（不知道是褒义还是贬义）。

calvin 的核心观点是，2PC 里面的锁是一个很糟糕的东西，应该避免掉 2PC。它的方式是弄了一个事务协调层，**把事务之间不确定的时序关系转化成确定的**，后面就可以直接走 log replication 了。所以它是 事务排序 + log 的流派。

calvin 避免了 spanner 那种做法在两阶段提交那一层的等待，但是它引入了事务定序那一层，所以这两种模型谁优谁坏其实不好说。不过 calvin 有一个很致命的问题在于，**必须预先知道事务的全部修改，这样协调器层才能够排序**。这就导致 calvin 模式无法支持交互式事务，也就 begin, stmt1, stmt2, stmt3 ... commit 这种。这个弱点太要命了。

cockroachdb 本来是灵感来自 spanner 的，然后这一篇优化呢，它其实是朝着 calvin 的方向演进。应该说，它结合了两者的长处，非常完美（当然 spanner 流派也可以说，我们中出了一个叛徒）。
