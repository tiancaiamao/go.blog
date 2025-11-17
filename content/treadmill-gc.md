读到一篇很好玩的文章 [treadmill gc](http://www.cofault.com/2022/07/treadmill.html)。
这应该是我读过的**最简单**的一篇能实现 non-STW 的垃圾回收的算法。很具备可行性，所以就激起了我的兴趣。

背后是来自于一篇 92 年的论文《The Treadmill: Real-Time Garbage Collection Without Motion Sickness》。

博客和原论文都是比较好读懂的，所以呢, treadmill GC 算法本身的基本内容只简单提一下。

treadmill GC 本质是一个结合了基本的三色标记，和基础的 copying GC 的算法，吸取了两者的优点。
不过相比 copying GC，它是一个 non-moving 的算法，用链表管理对象。
做增量增加回收一定会用到三色标记，所谓增量，就是 mark-sweep 不用一口气做完,中间可以停下来做分配操作。
因为中间可以停下来，两种颜色是不够的，需要有黑/白/灰三种颜色。灰色用于一个对象本身被标记了,但是它的孩子节点没被处理完的情况。等孩子节点也处理完了,就可以把该对象标黑了。

基础的 copying GC 算法，相对于 mark-sweep 算法，它没有 sweep 阶段，把 copying(类似于 mark 阶段)做完之后，回收就完成了。

treadmill 怎么样将两者结合起来，取其精华，去其糟粕的呢？就是把黑/白/灰三种颜色，变成了三个链表。颜色标记，变成了节点在链表之间的移动。
三色标记结束之后，灰色就没有了，剩下白色是没有被使用到的，和黑色是被使用到。这时两条链表交换，就像 copying 完成的时候的交换一样，旧的 white 就变成了新的 freelist。

因为需要区分，上一轮 GC 完之后没被使用到的，和当前的 freelist，它们都算白色，但并不是同一种颜色，所以 treadmill 用了第四种颜色 ecru。
它把四条链表串成了一个环，black (scan)的，white (freelist)，gray (to scan), ecru。
串不串成环，其实在我看来不是重点，用四条链表仍然是可以工作的。

优点：
- 简单，简单，简单！
- no stop the world 的增量垃圾回收

缺点：

- ~~the heap has a fixed size;~~

原论文和那篇博客中说，这个算法的主要缺点之一，是只能支持固定大小的对象。这其实是不对的。
它们都把 GC 和分配器实现混成一套了，强耦合。性能是好些，但是不利于对概念的理解。我曾经也犯过这样的错误，不过后来悟了。

**GC 只管理生命期，不管理具体的内存分配。**这才是对 GC 正确的理解。

解耦之后，就可以解决 treadmill 里面的只能分配固定大小的对象的问题了。具体的处理方式是，white 的 list 里面是各种不同 size 的对象，额外维护一些不同大小的对象池；分配的时候，如果待分配的对象的大小，正好等于当前 white 的头节点的 size，则分配头节点；否则把这个头节点放回到对象池里，再从对象池中，取出来一个正确大小的对象。过程还是 O(1) 的。

只要把解耦做好，事情就解决了。太多的介绍 GC 的文章，把生命期管理和内存管理搞到一起，就导致写个 demo 算法都特别困难，理解 GC 也就变得复杂了。
其实理解 GC 最好的形式，就是自己照着经典实现写几个。GC 真的不需要跟编程语言，跟 root 区域，跟内存管理耦合到一起。

它应该是一个这样的接口:

```
struct GC *gc = gcInit(allocator, allocFn, recycleFn);
gcRegistForType(gc, 0, consGCFunc);
g = gc;

scmHead* x1 = cons(atom(1), cons(atom(2), atom(3)));
scmHead* x2 = cons(x1, atom(5));
scmHead* x3 = cons(atom(1), cons(atom(2), atom(3)));
scmHead* x4 = cons(x1, x3);
scmHead* x5 = cons(x1, atom(6));
scmHead* x6 = cons(x1, cons(x2, cons(x3, cons(x4, cons(x5, x6)))));

// Try to GC something.
markRoot(g, x3);
gcStep(g, 1);
gcStep(g, 3);
gcStep(g, 5);
```

一些初始化之后，cons 分配的对象全部是由 gc 管理着生命期的。
但是分配和回收的具体操作，分配器是注册进去的，gc 会调它们去分配和释放对象。

```
void *allocator;
void* (*allocFn)(void* allocator, int sz);
void (*recycleFn)(void* allocator, void *ptr);
```

我们要标记哪些对象 GC 之后还需要使用着，也就是 `markRoot` 这步操作，然后就可以做 GC 了。GC 完成之后的效果是，所有还活着的对象，都不会被释放，而所有不再被引用到的对象，全部被释放回分配器了(至于分配器层是直接释放对象，还是回收到分配器自己的池子里重用，这个由分配器去决定)。

因为这个算法是个增量垃圾回收算法，所以可以做成 gcStep 由用户手动去调，每次回收一部分垃圾。假设不是增量的，那么要一次性把 mark-sweep 全部做完，就会有 stop the world，性能敏感场景导致卡顿。

其实 treadmill 真实缺点也有：

- 比较费内存
- 链表对硬件的 cache 友好性不咋滴

但并不是什么致命的问题。考虑到这个算法的简单性，其价值还是非常高的。我写了一个 demo 代码放在[这儿](https://github.com/tiancaiamao/cora/blob/639e062ec509f53132774fadc5087c788b394657/runtime/gc.c#L250-L287)。
暂时还没用到 cora 里面去，不过说不准哪天，就用它替换掉上次 [200行实现 copying 垃圾回收](/200-lines-gc.md) 那个丑陋的 stop the world 实现了。

