网上不少缓存设计的文章，讨论先更新数据库，还是先更新缓存的问题，其默认的设定是最终一致性。这一篇想讨论的，是在读多写极少的特定场景下，如何将多份缓存做成强一致的。

假设我们有一份原始数据，叫做 single source of truth，然后这个数据有多份的副本，也就是缓存。缓存的作用主要是应对热点读问题，single source of truth 扛不住所有并发请求打到一个点。
读的时候读取缓存，命不命中后面讨论，先说更新的问题。假如原始数据更新了，而缓存并不知道，那么就不是强一致了。

如果想让所有缓存的副本，同时知道某个写入操作怎么办呢？首先想到的，是让所有缓存副本都达成共识，比如说大家都先对需要更新的 key 缓存暂时失效，等更新完毕再重新 load 之类的。但是达成共识这个事情就涉及到一个分布式问题了，能做的只能走两阶段提交。协调者第一阶段先让所有缓存副本达成 “某个 key 需要更新了，你们做好准备” 的共识，如果能收集到所有的回答都是 ok，再做第二阶段由协调者通知所有参与者 “ok 可以执行了”。 这里面是有很多黑暗面的，尤其是某个节点一直不 ok 或者协调者死掉了之类的情况，分布式最讨厌的就是异常处理，然而异常又是必然事件。

换一个角度，我们可以做一个 lease。这个 lease 是承诺原数据在 lease 期间内都不会执行更新，这样缓存那边就可以放心大胆的使用了。具体点，加载数据到缓存的时候，源数据那边会加上一把读锁，这个锁是有记录有效时间的，比如 3秒，原始数据在这 3秒内就是只能读，不能修改。

缓存可以有许多份，它们可能加载了同一个 key，我们只记录 lease 有效时间的最大的那个值，比如说 1秒内不能改；2秒内不能改；3秒内不能改；我们只需要记最大的这个 3秒内不能改。每一份缓存相互不知道其它缓存怎么更新 lease 的，它只需要关注自己的读锁的 lease 时间有没有过期，若没有过期则读缓存的数据一定是安全的。在 lease 时间过期之前，缓存需要对 lease 续租，也就是再发一个请示到原数据，把读锁的时候再往后推几秒。

这里 “锁” 的概念跟操作系统的锁的概念不一样，这个锁就是 key 的记录中对应了一些额外的内容，要求在 lease 时间内只能读不能改。
只能读不能改，这并不是我们想要的。接下来，看看怎么样做数据的更新。

我们有一个带有效时间的读锁，在有效期内数据是不会被修改，所以缓存那边读很安全。为了支持写，我们要把读锁升级成写锁。协议这样子设计，写入方写 single source of truth 的时候，如果它发现对应的 key 上面有读锁并且 lease 时间还没到期，它需要先把读锁，修改成写锁标记，并且这个时候它还不能直接更新。读锁的 lease 时间会返回给写入方，它需要等待 lease 时间到期之后，再写入一次，并且这一次完成操作后要释放自己的写锁。

写锁也需要带上一个 lease 时间，防止客户端加完写锁后就 crash 掉了，然后写锁永远卡住其它操作。

最终的伪代码逻辑，读取，缓存层处理：

```
kv, ok = read from cache
if ok && now < kv.lease {
    return kv
} else {
    lease = now + 3s
    kv, ok = read from source (lease)
    if ok {
        write to cache
    }
    return kv
}
```

写入，db 层处理，场景一，kv 无锁

```
store kv to db
```

场景二，kv 被加了写锁

```
if kv.flag == write lock {
    // lock invalid
    if kv.lease < now {
        store kv {flag = invalid}
        return success
    }

    if kv.owner != caller {
        return retry later, blocked
    }

    store kv {flag = invalid}
    return success
}
```

场景三，kv 被加了读锁

```
if kv.flag = read lock {
    if kv.lease < now {
        store kv {flag = invalid}
        return success
    }
    
    // upgrade read lock to write lock
    store kv.flag = write lock, kv.owner = caller, kv.lease = now + Ns
    return retry later
}
```

读取，db 层处理，只说说遇到读锁，其它略过了：

```
if kv.flag == read lock {
    store kv.lease = now + 3s
    return kv, ok
}
```

空 key 的处理。处理缓存的时候是要注意空 key 这种特殊情况的。源数据中不存在，如果缓存层不缓存这个信息，就有可能大量的击穿缓存层；如果缓存层加载，那么源那边需要存储相应的额外信息。

写入的性能分析，首先是会不会有频繁的读操作，导致一直无法写入？不会。因为写操作可以把读锁升级成写锁，升级之后，缓存就暂时不能续租自己的读锁 lease 了。如果 lease 时候设置的是 3s，那么意味着写入操作最多需要等 3s，因为读锁升级成写锁后，还需要等待旧的 lease 过期，不能立即写入。当然，平均不一定会等待 3s，可能某个时刻是没有人在读的，那写就可能立即写，又或者 lease 到期时间不足 3s 了。

读需要更新 lease，那么有没有可能产生频繁的写入压力呢？应该不至于。因为这个 QPS 主要取决于缓存副本数量，以及 lease 的频率，假设 3s 的 lease，1/2 的 lease 续租一次，有 10 份副本，那才 6.67 的 QPS

最后的问题讨论，全局时钟和单点故障。lease 的那个时间是需要全局时钟的，否则会有本地时钟漂移问题；单点故障需要底层的 single source of truth 是一个容灾的系统。感觉在 TiDB 里面正好都不存在，可以考虑用于热点小表的优化。