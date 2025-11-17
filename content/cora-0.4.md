[时隔半年](cora-0.3.md)，I'm thrilled to announce 终于是时候发布 cora 的 [0.4 版本](https://github.com/tiancaiamao/cora/releases/tag/v0.4.0)了。这是一个里程碑式的版本，这门语言的成熟度进一步提升。

其实目前的发版没有什么特别的规划，就是想到啥做啥，当积累了足够的变化之后，就发一个版本，并非按时间来发版本。为什么算一个里程碑式的版本？因为我之前说，等我能够用自己的语言实现自己的博客的时候，说明语言的成熟度已经差不多了，到时候就发布 1.0 版本。如今已经做到了一半，我的博客[又](/go-org-mode-blog.md)[双](/chicken-scheme-practice2.md)[叕](/clojure-blog.md)重写了，这次有一半是用 cora 实现的。

## 博客重写

博客重写我拆成了两部分，一部分是从 markdown 生成到 html，另一块是实现一个简单的静态 http 服务器。拆成两半来做可以简化复杂度，一步一步达成目标。目前从 markdown 到 html 是用 cora 实现的，而静态 http 服务器是用 rust 实现的。

具体的过程跟之前 clojure 版本实现的时候差不多，都是从 markdown 然后成 sxml 的模板语言(DSL)，然后再生成到 html。sxml 是非常适合作为 html 的 DSL 的，在 0.4 release 里面，解析 [markdown](https://github.com/tiancaiamao/cora/tree/d90ad9e1604a2ea03327d95d436a77b0140b64ac/lib/md4c) 和 [sxml](https://github.com/tiancaiamao/cora/blob/d90ad9e1604a2ea03327d95d436a77b0140b64ac/lib/sxml.cora) 的库都支持了。

至于 http 服务器部分用 rust 实现，是因为最近这阵我正在重新学习 rust，正好借这个机会练一练手。一个非常 impressive 的印象是，rust 语言是真快，快到没朋友。我能明显地感觉到这次重写之后的网站访问速度变快了，没有实测数据，就是体感上。
当然，除了语言的影响，这也跟实现策略有关，之前是来了一个 http 请求之后，clojure 动态去从 markdown 生成到 html 并渲染到客户端，而当前是直接读取静态的 html 文件。

博客重写改得比较急，还遗留了一些 bug，比如 rss 输出暂时没实现；博客文章里面的"上一篇" "下一篇" 这样的翻页按钮挂掉了；静态图版本的链接还没处理对；还有刚刚发现一个 url 里面出现中文就没法访问...都是小问题，等后面修修补补就可以处理掉。

最终我会把 http server 也完全用 cora 重写掉，估计就下次或者什么时候，这样就是完全用 cora 实现自己的博客了，不过感觉也不会发到 1.0 版本，只会到一个 0.x 的版本。因为 1.0 之后对语言的兼容性会有更高要求，就要尽量避免破坏性的改动了。在此之前的版本则可以快速推倒，或者说叫瞎 JB 改。

## 版本的主要变化

0.4 版本最大的变化主要是两处：[分代垃圾回收](/cora-generational-gc.md)，以及[模块系统重新实现](/define-library.md)，都已经分别写过博客了，细节就不展开了。这两处改动都是工作量比较大的，前者是因为正确性太难搞了，修修补补了好久才[把 bug 都消灭掉](/generation-gc-bugs.md)。后者一方面模块重实现本身的工作量，另一方面是需要修改之前所有已有模块到新的实现。

模块当前使用了关键字 package，而不是像 scheme 那样用 define-library。可能我更喜欢 Go 吧，用 package 关键字可以少打几个字。

```
(package "path/to/cora/file"
  (import "another/package")
  (export fact)
  (func fact
	0 => 1
	n => (* n (fact (- n 1))))
	)
```

值得一提的是，相对于前面的[模块系统重新实现](/define-library.md)那篇博客描述，我最终还是把模块系统完全做到了宏那一层，只不过是宏展开完毕之后，再追加一个 stage 去处理。这样的好处是在 kernel lambda 部分仍然保持极简。if do lambda let 几个 special forms，set 都不用是 special forms 而是函数，let 则是为了性能而加入到 special form 的。

对应的设计哲学是：cora 的上层语言是一门宏语言，提供便利的语法，是对接用户的；而底层是极简的 kernel lambda，对接编译器的。上层语言通过"宏"这样的机制来对接到底层语言。本质上就是 scheme 那套做法。即使 kernel lambda 重新实现，从用户语言到 kernel lambda 这一层还是完全可以重用。就像 shen 语言那样，它是可以对接到许多语言的。不过 cora 对于 kernel lambda 有一些要求：必须支持尾递归，必须支持 curry/partial apply。上层语言到下层语言的过程，虽说是通过宏来支持，但是更像是某种编译过程。cora 的宏不是"卫生"的，这一层的作用是 sexp 改写，更类似于 staged 编译。

其它的小的改动就比较不值得展开了，只列一下：

- REPL 里面，支持解释执行和编译执行的混合模式，解释和编译后的代码可以相互调用 [#107](https://github.com/tiancaiamao/cora/pull/107)
- 更新了 PEG 的库，改成了类似 parser combinator 形式
- gensym 生成临时变量重新实现了 [#101](https://github.com/tiancaiamao/cora/pull/101) [#104](https://github.com/tiancaiamao/cora/pull/104)
- 重新支持了将 cora 作为脚本运行
- 修复 lib/cml 调度器不是尾递归，导致内在泄漏和暴栈的问题 [#99](https://github.com/tiancaiamao/cora/pull/99)
- 添加了一个简单的 trace 功能 [#97](https://github.com/tiancaiamao/cora/pull/97)
- cora 的栈对象使用 GC 托管的分配，也是为了处理泄漏 [#95](https://github.com/tiancaiamao/cora/pull/95)
- 支持分段栈而不是固定大小 [#94](https://github.com/tiancaiamao/cora/pull/94)
- vector 添加了一个 capacity 字段
- 添加 json 库，sxml 库 [#90](https://github.com/tiancaiamao/cora/pull/90)[#69](https://github.com/tiancaiamao/cora/pull/69), markdown 库[#68](https://github.com/tiancaiamao/cora/pull/68)[#64](https://github.com/tiancaiamao/cora/pull/64)
- 将 car/cdr 等函数调用生成宏以优化性能 [#88](https://github.com/tiancaiamao/cora/pull/88)
- 重构C代码生成那边的最后的分组过程 [#82](https://github.com/tiancaiamao/cora/pull/82)
- load 函数去掉了 pkg 参数 [#78](https://github.com/tiancaiamao/cora/pull/78)
- 字符串基础类型改名叫 bytes 类型了，不要跟 utf8 有误会 [#65](https://github.com/tiancaiamao/cora/pull/65)
- gc 支持大对象(>4K)分配 [#63](https://github.com/tiancaiamao/cora/pull/63)
- Makefile 中提供 make fmt 来统一代码风格 [#62](https://github.com/tiancaiamao/cora/pull/62)
- 区分全局变量和全局符号，GC的时候只处理变量而不是处理整个符号空间 [#61](https://github.com/tiancaiamao/cora/pull/61)

## 测试加强

- 1 million coroutine 测试

网上有一篇[文章](https://pkolaczk.github.io/memory-consumption-of-async/)，测试不同的语言，支持 1M 的并发任务，需要多少内存。这激发了我的好奇心，如果在 [cora 中做这个测试](https://github.com/tiancaiamao/cora/blob/d90ad9e1604a2ea03327d95d436a77b0140b64ac/test/benchmark/coroutine1m.cora)，结果是怎么样的？

cora 跟 Go 一样也是有栈协程，不过是分段栈而不是连续栈。初始的栈大小当前是 [254 对象](https://github.com/tiancaiamao/cora/blob/d90ad9e1604a2ea03327d95d436a77b0140b64ac/src/runtime.c#L31)，在 64 位机器上每个对象是8字节，于是 254*8*1M 大约 2G，除了计算栈，还有返回地址的栈(continuation 的概念)，也会有一点点内存消耗。实际测试下来，在 cora 中 1M 的协程最终的内存消耗大约是 2.6G 的样子，跟 Go 语言相当。Go 语言 2K 的栈跟 254*8 其实差不多大小。为什么是 254 而不是 256，为什么不弄个整数？其实故意的，因为协程的栈内存现在也是从 GC 分配，而 GC 那边分配的对象大小会向上取整，然后还有对象头的结构的一点点消耗，如果正好超过 2048 就会导致向下一级 4096 分配，空间浪费极大。而如果故意用 254 而不是 256，则可以对应到 2048 这一级大小。非常关键，又非常小的一个点，直接让 1M 协程的内存代价在 5.3G 降到 2.6G。

- ping-pong 测试

这个测试我折腾了好久才跑过，测试内容就是起两个 coroutine 通过 channel 不停地相互发送 ping pong，观察内存情况。
这个测试暴露了好多好多 GC 实现 bug 以及内存泄漏的问题。比如说发现了调度器那边不是尾递归，导致了 schedule 切换的时候栈大小会一直涨。最最最让我无语的是测试 case 本身写得有问题，导致 channel 消息队列一直堆积消息导致的内存无限涨，这个排查了好久。因为一直怀疑是 GC 那边的实现问题，以为是那边有什么东西分配了没释放或者有什么问题造成回收不掉。

- [stackoverflow 测试](https://github.com/tiancaiamao/cora/blob/d90ad9e1604a2ea03327d95d436a77b0140b64ac/test/benchmark/stackoverflow.cora)

在固定栈大小，并且没做分段栈之前，这个测试就过不了:

```
(func f
      0 x => 0
      n x => (+ (f (- n 1) x) x))

;; test no stack overflow
(f 1000000 1)
```

实现分段栈之后也就修掉了。还是 ack 函数测试，之前跑不过，也是因为爆栈的。

这一波测试加强，让我对 GC 实现更有信心了。测试是需要重视起来的，不然等代码规模更大以后就维护不动了。当前是加了一些测试，不过还比较混乱，后续得把 CI 搞起来，提交代码要自动跑 CI，而不是自己手动去跑测试。题外话，我在 linux / mac 不同的系统上都有跑过 cora 的测试，并且也涉及不同的 cpu 架构，有 x86，有我的[玩具 arm 机器](/smart-am40.md)，也有 mac 的 m3，还有在 vps 那种虚拟环境，得益于 C 语言的良好跨平台性，cora 编译到 C 基本上在所有平台和系统上都跑得很欢快。

## 后续畅想

cora 0.4 版本已经得到了极大的增强，实际的证明就是至少博客有一半是用 cora 重写了。http server 部分需要的稳定性更高，需要一直跑着保证没有内存泄漏。然后涉及的库也更多，主要是 coroutine / network 这些，还需要继续完善。
后续版本中要处理的，这里想到的，随便记录一些：

- http服务器
- partial evaluation
- 多核支持
- 补测试
- gc的tuning
- 等等等

partial evaluation 是一个很有意思的性能优化话题，[guile 那边的博客](https://wingolog.org/archives/2011/10/11/partial-evaluation-in-guile)可以参考，包括里面推荐的 [Partial Evaluation Tutorial](https://www.cs.utexas.edu/~wcook/tutorial/)，还有 Kent Dybvig 的 [Macro-Writer's Bill of Rights](https://www.youtube.com/watch?v=LIEX3tUliHw) 视频也是学习材料。我还找到了一个 Marc Feely 的 [peval 的代码](https://www.cs.cmu.edu/Groups/AI/util/lang/scheme/code/eval/peval/peval.scm)。

[多核支持](/cora-multicore-support.md)之前也写过想法了，总体来说还是没想清楚，所以不急于实现。

GC 的tuning这块还是有空间的，我发现什么时候触发GC，要回收多少，minor 还是 major，等等等策略相关的东西，其重要性甚至不低于实现 GC 算法本身。另外，现在还有两个问题，一个是GC的回收只是回收到了 runtime，怎么样去归还给操作系统还没处理。另外就是关于并发，支持并发让我很抗拒，因为实现复杂度高了之后 hold 不住，但是重要性又很强，如果能想到好的方案，也不排除这块继续优化。


