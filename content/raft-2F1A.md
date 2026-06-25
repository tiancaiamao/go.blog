raft 在一致性这个领域占据了主导地位。打败 paxos 的原因是它理解简单，实现简单。
但是它是有代价的，三副本的成本比较高。能不能在两副本的情况下，达成高可用强一致？
这就是 2F1A，即 2副本1仲裁(arbitrator)。

raft 配置两副本能不能跑呢？能跑...但是遇到挂 1 副本之后，就会 quorum 无法达成 majority，然后不可用。
3 副本没这个问题是因为 3 副本挂一副本后，剩下 2 副本仍然构成 majority。

解决这个问题有两个方向：

- 当 2 副本挂掉 1 副本后，我们将 raft group 缩到 1 副本，仍然可以满足 majority 提供服务
- 弄一个投票而不存数据的副本出来组建 3 副本，这样两副本挂 1 个剩下的仍然满足 majority

这两个方向对于 raft 都很难。

先解释前者。可能有人问，如果我们把 raft 退化成一个主从，同步复制的主从。
一旦有一个节点挂掉，有一个仲裁节点把挂掉的那个踢掉，活着的变成单主。这个概念上很好解释呀！
实现上到底有什么不可行的呢？

让我们先听听 AI 怎么说。

## 问题 1：你怎么知道它"挂了"？

**直觉**：B 超时没响应 → B 挂了 → 踢掉。

**现实**：B 没挂，只是网络分区了。B 活着，还认为自己是 secondary，还在接受旧的 primary
的同步。

```
实际场景：
A (primary) ←─网络断了─→ B (secondary)
     ↓
A 联系 Arbiter："B 不响应"
Arbiter 同意踢掉 B
A 变成单主，继续写
     ↓
同时 B 这边：
B 联系不上 A，但 B 的本地数据是完整的
如果 B 也能联系上 Arbiter → B 可能也发起"踢掉 A"
```

**这就是脑裂**。两个节点都认为对方挂了，都认为自己是合法的主。

**解法**：必须有一个**强一致的裁决协议**。Arbiter 不是"裁判喊谁赢谁就赢"，而是 Arbiter +
幸存者必须**共同完成一次共识投票**（Paxos prepare/promise
或等价机制）才能变更成员组。这个投票本身就是一个 mini 分布式共识过程。

## 问题 2：踢掉一个节点的操作本身需要共识 → 鸡生蛋

**直觉**：Arbiter 说踢就踢。

**现实**："踢掉 B" 是一次**成员组变更**，这个变更必须被持久化，且所有存活的参与者达成一致。

```
变更前：{A, B, Arbiter}
变更后：{A, Arbiter}（B 降级为 learner）

问题：这个变更日志谁来 commit？
- A 单独不能 commit（此时 majority 还是 2/3）
- 需要 Arbiter 参与投票才能达到 majority
```

所以 Arbiter **必须参与配置变更的 quorum**。这就是为什么 OceanBase 的仲裁成员虽然不参与日志
commit，但**必须参与配置变更投票和 leader 选举投票**。

## 问题 3：切主时的日志连续性 — 到底 commit 到哪了？

**直觉**：同步复制 → B 有完整数据 → B 直接上。

**现实**：考虑这个时间线：

```
t1: Client 发写请求给 A
t2: A 写本地日志 [100]
t3: A 把 [100] 发给 B
t4: B 收到 [100]，正在刷盘...
t5: B 返回 ack 给 A（"我持久化了"）  ← 这个 ack 在路上
t6: A 还没收到 ack
t7: B 炸了

问题：[100] 到底算不算 committed？
- A 的视角：没收到 B 的 ack → 没 commit → 客户端应该收到失败
- B 的视角（如果 B 没死，只是慢）：已经持久化了 [100]
- 如果 B 恢复后当了主 → [100] 存在但客户端以为失败了 → 幻读
```

同步复制中，**"同步" 这个词的定义本身就是核心设计决策**：
- 收到 B 的 ack 才算 commit？→ B 慢就全卡
- A 本地持久化就算 commit，B 异步追赶？→ B 故障时可能丢数据

**OceanBase 的解法**：quorum-based。正常时需要 A+B（majority(2)=2）才 commit。B
挂了之后，先完成 degrade 配置变更（需要 A+Arbiter 投票），然后 `log_sync_replica_num=1`，A
单独就能 commit。**degrade 操作完成之前，A 不能 commit 新日志**（代码中
`is_changing_config_with_arb()` 检查）。

---

## 问题 4：老主 fencing — 怎么保证它不再写？

**直觉**：B 被踢掉了，A 是新主。

**现实**：如果 A 没有立刻知道自己被踢（比如 A 其实只是网络分区，没挂），A
可能继续接受客户端写请求。

```
真正的故障场景（不是 B 挂，而是 A 和 B 之间网络断了）：

A 的视角：B 不响应 → 联系 Arbiter → degrade B → 单独 commit
B 的视角：A 不响应 → 我也联系 Arbiter...

谁能先联系上 Arbiter，谁就活。
另一个必须被 fence（ fencing）。
```

**解法**：
1. **租约机制（Lease）**：leader 有时间限定的租约，过期前不能被抢
2. **Epoch / Term 机制**：每次切主递增 term，旧 term 的写操作被拒绝
3. **客户端 fencing**：客户端发现新主后，旧主的写被拒绝

这些都不复杂，但**每一个都必须正确实现**，漏一个就是数据损坏。

## 问题 5：故障节点恢复 — 追赶期间的正确性

**直觉**：B 恢复了，追一下日志就行。

**现实**：B 被踢掉期间，A 单主写了 [100, 200)。B 恢复时：

```
B 本地日志停在 [100]
A 已经写到了 [200]

情况 1：B 只是网络恢复（没重启）
  → B 可能还以为自己是 acceptor
  → B 可能发起选举（如果 term 过期）
  → 需要正确地被降级为 learner，安静地追日志

情况 2：B 重启了
  → B 读本地 meta，发现自己已经被移出 log_sync_memberlist
  → B 作为 learner 向 A 请求 [100, 200) 的日志
  → 追平后，upgrade 回 acceptor
```

追赶期间，B 不能参与 quorum（否则可能把未追平的状态带入 commit 决策）。这就是
`degraded_learnerlist` 存在的意义。


| 问题 | 一句话 | 做不好的后果 |
|------|--------|------------|
| 怎么判断挂了？ | 不是超时就踢，需要 Arbiter+幸存者共识投票 | 脑裂，双主写 |
| 踢人需要共识 | 配置变更本身需要 Arbiter 参与投票 | 变更丢失，回退到旧配置 |
| Commit 到哪了？ | Degrade 完成前不能 commit 新日志 | 已提交数据丢失 |
| 老主怎么办？ | Term/Lease fencing | 老主继续写，数据分叉 |
| 恢复时怎么追？ | 降级为 learner，追平再升级 | 脏数据参与决策 |

这不是不可行 但这不是 "加个 arbiter 就完事了" 的简单工程。
它是一个完整的分布式共识协议的变体。上述 5 个问题，每一个都需要精确的设计和实现。

现在不看上面的内容，再回到 raft。具体到 raft 两副本的时候，有几个问题就是挺不好处理的。

首先集群成员变更。在 raft 的 conf change 中，join consensus 有描述变更的处理。
它是要求变更前的配置，和变更后的配置，两者能同时达成 majority。

比如 3 副本情况下 {1, 2, 3} -> {2, 3, 4} 把 1 拿掉，把 4 加进来，挂任何一个点，前后配置还可以满足 majority。
但是 2 副本情况下 {1, 2} -> {2, 3} 变更过程中, 只要挂掉 1 个，无论挂哪一个，都会出现前后配置无法达成 majority，
这时就会进入不了新配置，又回退不了旧配置，卡死的情况。

再一个场景，比如集群滚动升级。如果我们是三副本的时候，可以先停一个节点，升级到新版本。升级后再停一个再升级，
一直可以滚动到新版本，过程中不中断服务。但是到了两副本的情况下，容忍的前提是 reconfiguration 把副本数降到 1 从而恢复可用。
但是滚动升级这个场景，滚动过程中不好保证继续提供可用性。因为不好提前升到 3 副本容错，或者提前把 raft group 降到 1 去把待滚动的节点踢出去。

还有就是两副本之间，出现网络隔离，但是它们跟 arbiter 都是连通的。
这个场景 arbiter 很难介入去调整。两个副本都因为拿不到 majority 投票降为非 leader，又选不出 leader 来。
即使我们去做一些处理，比如 A 无法拿到 B 选票之后，去 arbiter 那边要求降级配置，
然后 arbiter 回复同意，只要 arbiter 同时了 A 的降级请求，就不再接受 B 的请求，来防脑裂，
这也是一个小型的共识协议了。

为什么会这么绕呢？本质还是因为 raft 协议根本没有包含写两副本怎么样处理 conf change 这个事情！

那我们换一个思路行不行。前面说了，另一条路是弄 3 副本，然后其实有一个副本是假相嘛，这样挂 2F 中的一个之后，
剩下的还可以构成 majority。

raft 协议里面是有一些特殊的角色的，比如说 learner，它不参与 quorum 投票，其它跟 voter 差不多。
这个角色主要是为了 conf change 的时候，不用从头追日志，从 learner 到 voter 可以迅速完成角色变更。
raft 协议里面还有一个叫 witness 的角色，按照原始论文里面的说法，witness 角色是参与投票的，但是不存数据。

tikv 之前实现过一个版本的 witness，并且是按 raft 原始论文来实现，但是实践中证明会有一点问题。
这个坑就是 witness 存了日志。假设现在我们有三副本，{A=leader B=follower C=witness}。
A 只要日志收到 majority 回复，它就是成功了。假设 B 的网一直很差，或者由于 B 要处理数据 apply 执行得慢一些，
很容易就是 A 和 C 一直往前走，而 B 远远落后。这个时候 A 挂掉了。根据 raft 协议，日志最长的会成为新的 leader。
如果让 witness C 成为了新的 leader，C 上面是没有数据的，它会无法服务！
如果不让 witness 成为 leader，那么就必须让 B 追上 C 的日志，这个追日志的过程，可能很慢，这个日志落后多少甚至是无上界的。
真实的场景中，这会造成不可用。

我就发现，不管怎么样，都很绕。raft 实现 2F1A 好像很难搞！那为什么 oceanbase 据说是实现了 2F1A 的呢？

答案藏在 raft 和 paxos 的差异里！

**oceanbase 的 election 和 commit 可以用不同数量的 replicas**！在正常 2F1A 时，OB 它是 commit 维护 2 replicas 的 group，而 election 维护 3 replicas 的 group。
1A 它属于 election group，但是不属于 commit group。于是可以 2 份数据。
出现 conf change 降级的时候，election 由 1A 和另一个活着的 peer 可以达成 majority。
降级后，维护 1 replicas 的 commit group，而 election 是 1A+1voter 的 2 replicas group。

raft 的 leader 选主，以及 log 都是属于协议本身的，一旦形成 group，就要么固定 3 副本，要么固定 2 副本。
3 副本的时候，无论是 witness 还是什么角色，都得维护 log，维护 log 就得追 log。维护 log 是保护 raft 协议正确性的要求。
2 副本，这就导致一旦要 conf change，就涉及重新选举没法 majority，这个就不在 raft 协议层内，而需要外部 hack。

如果在 2 副本加外部 arbiter，那这个 arbiter 就不属于协议内部，这就好比那种主备高可用的外挂一个共识服务，
无轮是 lease 或者检测不可用都变复杂，时间太短会误判，而时间太长则不可用的恢复时间增加。本质都是因为 arbiter 不属于 raft 协议内部的成员。

研究了许久之后，终于在 etcd 的一个 [issue comment](https://github.com/etcd-io/etcd/issues/8934#issuecomment-398175955) 里面看到一个非常精彩的回复：

> To the best of my knowledge, witnesses cannot be used with Raft because Raft requires that replicas maintain a complete prefix of the log. This prevents a replica that does not have the latest log entries from becoming leader, breaking fault tolerance if witnesses are used. A Paxos-based system could elect a node that is behind as leader, and have the new leader obtain the missing log entries by running Prepare-Accept logic, bringing it up-to-date.


顺藤摸瓜地，我找到了答案，[Extended Raft](https://github.com/etcd-io/raft/issues/133)！

就是 raft 原始论文里面的那个 witness 实现方式，不能够很好的解决 2F1A 的问题。
解决这个问题需要扩展 raft 协议。

extended raft 里面引入了 witness 角色，然后极具创新地引入 replication set 的概念。
传统的 raft 里面，有 membership 的概念， {A, B, witness} 这个是一个 raft group 的 membership，这个是不变的。

leader 的 replication set 是可变的。正常态，replicationSet = {A, B}，leader A 向 B 复制日志，majority{A B} = 2。witness 不参与。

当 B 故障时，leader 调整复制集，replicationSet = {A, witness}，majority 还是 2 的，B 不在复制集中，不参与。

成员始终是 3 个，但是通过动态调整 replication set，让 majority 总是可用。

引入 replication set 之后，就额外引入了一个 subterm 的概念。原本的集群变更，每一任 leader 会有 term。然后日志会有 index。
现在每一轮 replication set 的调整，会让 subterm 加1 以区别。日志的标识变成了 (index term subterm)

witness 里面是完全用存日志的，它只需要知识 leader 的复制集的状态。subterm 让 witness 能用最小的信息量判断 "日志新旧" 和 "谁是合法 leader"。

算法描述可以去看原论文，这里不展开了。结论这个确实是一个不错的两节点高可用强一致的方案，可以让 raft 协议下实现 2F1A。
