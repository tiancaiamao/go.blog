对于 [gambit](https://gambitscheme.org/) 的实现路径，现在我是越看越喜欢了。它设计了一个不太复杂的虚拟机 gvm，用于将底层硬件与上层解耦，将 scheme 编译到 gvm。
然后 gvm 可以对接到不同的语言，默认是生成到 C 代码，后来也做直接生成到 native 端的，[性能](https://ecraven.github.io/r7rs-benchmarks/)有一阵几乎超过 chez scheme 排到第一去了。

gvm 的设计并不复杂，还可以对接到很多不同的语言，像 javascript 甚至是 Go。从 [ribbit](https://github.com/udem-dlteam/ribbit) 里面，能看到不少 gvm 的影子，毕竟背后都是同一波人搞的。

读 ribbit 的时候，为了搞清楚 rvm 的虚拟机怎么工作的，顺带去把 gambit 相关的几篇论文都扫了一眼，一个重要的收获是，搞明白了 gambit scheme 的 call/cc 是怎么实现的。

gambit 不是做一个完整的 cps 变换，而是只特殊变换到能适配 gvm 虚拟机那一层。gvm 里面有 continuation 的概念，其实就是 pc 和 stack。jump/call 有两种形式，一种是直接跳转不带返回的，一种类似于 call，但又不是 call，而是把返回做成了 continuation。
调用过程是，进栈 continuation，进栈函数，再进栈参数，然后 jump 到 callee 的 pc 去。

由于 gvm 是一个暴露了 continuation 的设计，实现 call/cc 就可以通过这个特殊的虚拟机设计来做。

关于栈的处理，如果是做 cps 变换实现 call/cc，然后用 chicken 的那种搞法，等于所有的分配都会变到堆上面，给 GC 会带来不小压力，最终反映到性能层面问题。
gvm 的设计是在 vm 层暴露 continuation，但是并不做 cps 变换。然后使用了混合表示。[这一篇](https://www-labs.iro.umontreal.ca/~feeley/papers/ThiviergeFeeleySFPW12.pdf)里面有一张图，画得特别清晰了。

每一个调用，都有自己的 stack frame，**当不涉及到 call/cc 的时候，直接使用栈空间表示就可以了。而当涉及到 call/cc 的时候，把 stack 拷到堆上去，并且将 stack frame 用链的方式来表示**。因为 call/cc 毕竟是少数情况，这样的混合表示可以支持 continuation 同时达到比较好的性能。

我觉得 gvm 设计还有一个好的地方是，由于 stack heap pc 等等所有 vm 细节全部是自己控制的，它可以跟宿主语言的 call stack 环境共存。不管是以栈的方式，还是以 linked frame 的方式，都不需要用特殊的汇编代码去处理。
