许多年前，周sir 写过一篇这样的博客：回到过去，找回遗失的珍宝 -- [TiDB 的历史读功能](https://pingcap.com/zh/blog/time-travel) 。
虽然我们内部一直当一个梗来玩的，但是今天开会讨论用户需求的时候，又开了一个脑洞。

游戏那边的用户，其实对回滚到历史数据还是很有需求的。比如出了重大 bug 需要回档之类的。做定期备份之类的，但是只能回滚到备份时间点，而且定期备份对资源的浪费还挺大的。

要是能有一个时光机数据库就好了！可以在历史时间随意穿梭。怎么来设计这个东西呢？其实只要在周sir 的时光机方案上做一点儿改动。

数据库这边，其实核心就是把写操作变成 (raft) log，然后 apply log，得到数据。由于要处理事务，数据是用 MVCC 形式组织的。
只要不做 GC，MVCC 其实保留了所有历史数据的。这也是实现 TiDB 历史读功能的必要条件。


可以把 MVCC 数据想象成这样子的:

```
key1 -> v3 -> xx
     -> v2 -> xy
     -> v1 -> yz
     -> ...
...
key2 -> v2 -> 42
     -> v0 -> 56
```

如果我们要读 timestamp v2 的时刻的数据，得到的就是 key1=xy, key2=42。
如果读 timestamp v1 时刻的数据，则是 key1=yz, key2=56。

MVCC 的删除操作是很重的，因为删除是靠标记删除，然后做 compaction 的方式。
假设在 v4 时刻删除 key1，上面会变成:

```
key1 -> v4 -> DEL
     -> v3 -> xx
     -> v2 -> xy
     -> v1 -> yz
     -> ...
...
key2 -> v2 -> 42
     -> v0 -> 56
```

会留一个 DEL 的 tombstone 标记，读取的时候遇到它，就知道更早的版本不用看了。

TiDB 的历史读功能，set @@tidb_snapshot 之后，就会读历史版本的数据，而不是当时时间戳的数据。把 "历史读"功能，变成"时光机"功能，有两个问题没有解决。这两个问题分别是：

- 只读约束
- 性能问题


假设用户想回档到某个历史版本，只需要 set @@tidb_snapshot 就可以了，但是设置了 snapshot 之后，就无法支持写操作了。
用户想要的是，回档到这个时间点之后，可以继续写入; 以及更高级的，写入之后，还可以选择回到另一个时间。

很容易想到的一个处理方式是，set @@tidb_snapshot 之后，把数据全导出去，再在另外一个数据库中恢复出来。但是如果数据库很大的情况下，这个做法不怎么靠谱，比如说数据库有 10T，光是导出去就不怎么现实。

我们设想一下，如果把回到某个历史版本，把数据导出去，再回到当前，把数据导回来，得到的 MVCC 是什么样子的？它会是这样：

```
key -> v2022-08-11-14:33  -> dOld
    -> v2022-08-11-13:32  -> xx
	-> v2022-08-11-13:16  -> yy
	-> v2022-08-11-10:53  -> dOld
```

也就是说，实际结果等价于，把历史版本的数据取出来，然后以当时时间戳，形成 kv 再追加到最新的 MVCC 数据上面。
这个操作可以下推，由每一个 tikv，各个 region 去执行。它跟 2pc 执行事务差不多，只不过是一条特殊的 value 而已。
所以我们可以想象成，set @@timestamp = ts 是把整个库设置到一个只读的历史版本的状态，如果实现一条命令 flashback ts，则是把 MVCC 的那个时间的 value，作为新的值写到最新的 MVCC 数据上。

这个时间是可以随意跳的，比如说，在 12:50 的时候，将数据库变回到了 8:00 的状态，那 MVCC 记录就是：

```
12:50 -> v1
若干历史记录 ...
8:05 -> v2
若干历史记录 ...
8:00 -> v1
```

这个时候可以写任何的东西，之后变成了

```
13:15 -> xxx
若干历史记录 ...
12:50 -> v1
若干历史记录 ...
8:05 -> v2
若干历史记录 ...
8:00 -> v1
```

接下来，还可以回到 8:05 的时候，也可以回到 13:05 的时候。回到 13:05 相当于在 12:50 的时候回档到 8:05，回档之后又往进走了一会儿，现在仍然可以回档到 8:06，又或者是 13:15，或者是任何的时间，只要数据没有被 GC 掉。

**所以只要在历史读的基础上，做一个把历史版本追加写到最新版本的功能，就可以实现回档写入。只读约束就解决了。**

接下来看另一个问题，性能。为什么需要做 GC，因为不清理数据，会占用很多的空间。更要紧的是，会影响性能。当前 tidb 的 key 编码的布局是这种：


```
key1 -> v4 -> xx
     -> v2 -> xy
     -> v1 -> yz
     -> ...
...
key2 -> v2 -> 42
     -> v0 -> 56
```
如果更新很频繁的情况下，单个 key 的版本会特别多。这个时候做 scan 性能就不好了，rocksdb 那边要跳过非常多的 key，扫描的开销会变大。
假设我们把 latest cf 分离出来，扫描 key1 -> key2 就是性能开销常量了。这里有个 [RFC](https://github.com/tikv/rfcs/blob/d64a4a6120f79f1d290b56fe57110b984261f5f3/text/0095-add-latest-cf.md) 是想解决这种问题的。


如果这两个点都满足了，那事实上就可以实现一个时光机数据库了，只要盘够存非常多的历史数据，想跳到哪个时间点都可以。
当然，这两个点可能不那么好解决，所以先脑洞一下。
