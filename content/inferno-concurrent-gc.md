最初搞 multics 的那波人失败之后，搞 unix 搞成功了。然后他们觉得 unix 还是不够 “一切皆文件”，于是再搞 plan9 失败了。plan9 并不是设计理念上的问题，只能说商业化上面并不成功。其实 plan9 操作系统那波人，并不光搞了一个系统，而是顺带搞了很多好玩的东西出来，像现在的 Go 语言也可以算是一个副产物。其实在 Go 语言之前，它的前身还有一门叫做 limbo 的语言。那么这里说的 inferno 是什么呢？inferno 是由于他们搞 plan9 系统没搞成，就想在更上层搞事情，在系统之上另起炉灶，搞 VM。无论你是 mac windows 还是 linux，反正 inferno 系统统一再跑在这一层的上面。所以 inferno 是一个类似于 JVM 的东西，然后在用户层，实现 limbo 那样的语言，类似于 java。

好了，科谱完毕。今天的正题是写一写其中 inferno 的并发垃圾回收算法。具体是这篇 [paper](http://doc.cat-v.org/inferno/concurrent_gc/concurrent_gc.pdf) 《Very Concurrent Mark-&-Sweep Garbage Collection without Fine-Grain Synchronization》。

它是一个 mark&sweep 的算法，~~新颖的点~~(划掉，几十年前的 paper 了)是它的并发性，marker sweeper mutator 三者是可以在不同的线程中，然后同时跑着的。也就是说，内存分配跟垃圾回收并不相互影响，就有点像给飞行中的飞机更换发动机。我记得许许多多年以前看云风的文章，也有过类似的 idea，大致的想法是说现在的 CPU 这么多核了，其实只要拿出单独一个线程去搞垃圾回收的事情，一边同时在分配，一边同时的清理，并且没有 stop the world 的卡顿，浪费一点 CPU 是完全可以接受的。

那 inferno 的 GC 的算法是怎么样做到并发的呢？它引入了 epoch 的概念。每个对象都有对应的 epoch 标记，就像三色标记算法那样的标记。epoch 不停地往前涨，同时存活着的有三代的 epoch，当前的分配出去的对象一定是最新一代的 epoch，也就是对应 mutator 管理的；然后是 `epoch-1` 这一代，归属 marker 线程管理，marker 做的事情，就是从 root 区遍历，把还活跃着的对象的 `epoch-1` 标记改成 epoch；最后 sweeper 管理 `epoch-2` 的对象，也就是清扫将标记为 `epoch-2` 的对象归还到 freelist。

由于不同的 epoch 区分开了对象的归属，这里面几乎不涉及多少线程间同步。唯一会修改对象的标记的，只有 marker，它将活跃对象的标记从 `epoch-1` 变成 epoch。

`epoch-1` 的所有对象，经过 marker 扫描，如果是活跃的则被改成了 epoch，而如果非活跃则会到下一轮变成 `epoch-2`，最终由 sweep 回收掉。当 marker 和 sweeper 都一轮工作结束之后，达到了一个这样的状态：所有活着的对象，都被标记成了 epoch。处于 这个时候可以做一个短暂的 stop the world 的同步，处理全局状态变化让 epoch 往后涨。

epoch 这个事情，感觉很像我之前写过的[一篇博客](time-based-allocator.md)，假设对象生命周期大多是短暂活跃的，然后基于时间一类的东西，把活跃的对象保留下来，剩下的旧的对象直接回收。

------------

接下来我们要考虑的是，存在对象的更新的时候，这个时候需要对算法做一些修改。

假设上层语言是一门纯函数式的语言，只有分配没有副作用的话，到这里算法都可以很好的工作。举一些例子，我们看几个场景。新分配出来的对象的标记是 epoch。引用到的是同一代的对象也是 epoch，那么完全没问题。因为在这一轮，对象并不会被回收掉，而到了下一轮，假设新分配出来的对象还是活跃的，那它是可以保护自己引用到的对象的。

有可能把一个当前对象，修改到指向另外一个 `epoch-1` 的对象，这个对象原本是不活跃的，由于这个更新，有活跃对象引用它了，就需要保护它不被 GC。

如果不做保护，就有可能出现这样的时序：marker 扫描的时候已经经过它了，然后引用更新了，但此时 marker 不会再扫描一遍它，将标记从 `epoch-1` 变到 epoch。于是对象被漏掉了，后面就会引用的对象被 GC 掉。而且更新引用时，不光这个对象应该是活跃的，由它引用的所有孩子节点，也都应该变成活跃的。

怎么修改算法呢？答案是，在更新引用的时候，就把被引用的对象，重新再 new 一份，新分配的副本标记肯定是 epoch 的。然后不要引用老的对象，而是引用 new 出来的副本。最后一步，是把旧的对象加到 root 区里面（在 paper 增量的部分不叫 root，叫 store）。marker 那边要处理新添加到 root 的情况，这个虽然需要涉及线程同步，但是并不复杂。

--------------

root 区域的确定。

在一轮 marker 和 sweeper 结束之后，在 epoch 加加之前，这个时候所有活跃的对象标记都是 epoch，在这个点快照下所有的对象，就正好是下一轮的 root。

那整个算法启动之前呢？初始的 root 是空集。一轮之后 root 就不空了。

```
big int epoch = 2;
root_set_t roots = {};
root_set_t stores = {};
thread_t mutator, marker, sweeper;

forever {
	mutator <- make_thread mutate(stores, COLOR(epoch));
	marker <- make_thread mark(stores, roots, COLOR(epoch));
	sweeper <- make_thread sweep(COLOR(epoch - 2));
	barrier_sync {marker, sweeper};
	suspend_thread mutator;
	while (stores != {}) {
		resume_threads {mutator, marker};
		barrier_sync {marker};
		suspend_thread mutator;
	}
	/* invariant: all reachable data has COLOR(epoch) */
	roots <- get_roots(mutator);
	delete_threads {mutator, marker, sweeper};
	epoch++
}
```

整个算法卡顿的部分，就在于 `suspend_thread` 之后，对 stores 处理的部分。意思就是说，最开始是完整的 mark sweep，由于一边 mark 一边在产生新的，所以到完整的 mark sweep 做完之后，开始一轮一轮地处理增量，直到增量也为零了，就可以结束这一轮了。
