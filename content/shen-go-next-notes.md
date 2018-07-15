好久没更新博客了，[shen-go](https://github.com/tiancaiamao/shen-go) 也是停滞的状态。其实这期间一直有探索，如果更新下一版应该怎样做。

自己实现语言的编译是可行的，但是仅靠一人之力实现一门语言的生态是不靠谱的。如果没有好的库和生态，实现出来也就是一个玩具。我最期望的是能够做一个自己平时开发都会使用的语言。生态的构建，一种方式是像 lua 那样，做成一门嵌入式的语言，去借用宿主语言的生态。或者是支持方便的 FFI 扩展。在 shen-go 的 v0.3 里面，我实现了一个 load-plugin 的功能，可以用 Go 写代码，生成 plugin，然后 load 进去使用。

如果和其它 Go 语言实现的各种玩具 lisp 项目比较，这个项目应该是算很靠谱了，至少做了一个 bytecode 的 VM。然而这只是一个性能与复杂度的折衷阶段。当前的实现没法将性能压榨到我所想要的极限。在[这一篇](http://www.zenlife.tk/eval-as-universal-machine.md)里面，我已经说过了，其实并不想优化 VM。这个做法的性能是有尽头的，再往 JIT 方向做，复杂性又不能接受。

于是编译到一个现有的东西，无论是语言或者平台，似乎是更好的选择。

## 几个新的想法

Go 语言可以 load 插件，这个让我萌生一个想法：其实可以把 shen 编译到 Go 的源代码，然后生成二进制的 plugin，再 load 进来调用。在[之前的一篇博客](http://www.zenlife.tk/expression-implementation.md)里面，也已经提到过。这种 code gen 的方式，就等于是在做 AOT 编译了。

顺带说一说，Go 的 plugin 其实是有一点 ugly 的，我在看了下实现才发现，它是走的 cgo，调用 dlopen。cgo 是一个我希望避免的事情，普通的 Go 函数调用大概在 4-5ns，而一个 cgo 函数调用要到 70ns。那么其它方式直接 load Go 的程序执行？[当然也有人造过轮子了](https://github.com/dearplain/goloader)。

另外一个想法是关于 fixnum tagging。假设我实现了 shen 编译到 Go，那么，不做 fixnum tagging 肯定不能忍。这个优化对 runtime 的性能影响太重要了。编译成 Go 却不做这个，就像配电脑，配置了一个顶级的 CPU，结果只给它配了 256M 的内存条。

我甚至一度以为找到了一种 fixnum tagging 的简单实现方式。在 new 一个对象的时候，返回的是 uintptr，但是额外用一个地方记录下分配的对象的指针或者 unsafe.Pointer。比如用一个全局的数组记录。uintptr 不具备保护功能，但只要那个数组里面还有引用，就能保护该对象不被 GC 掉。

    func newCons() uintptr {
      ret := &Cons{}
      gcProtect = append(gcProtect, ret)
      return uintptr(unsafe.Pointer(ret))
    }
    
然而所有对象变成了都需要这个额外数组的保护。因为分配出去的 Cons 不是具备保护里面成员的功能的：

    type Cons struct { // uintptr 即使对象被引用也保护不了里面的 car 和 cdr 成员不被 GC
        car uintptr  
        cdr uintptr
    }
    
那么，如何回收 gcProtect 数组就成了一个问题。我需要遍历一遍 gc 的 root 区域，然后递归地 trace 里面的所有被引用对象。这就跟自己实现 GC 时，mark sweep 里面的 mark 阶段一样了，也相当于把 GC 自己实现了一遍。

最后得出了结论，除非自己手动实现一套 GC，否则 fixnum tagging 在 Go 语言里面是做不了的。

很排斥自己去弄 GC 的，复杂度偏高了。实现是一方面原因，更不爽的是，在托管内存和非托管内存之间交互，给使用会带来一些限制，最终会非常恶心。于是暂放下了这些想法，转而看能不能选择编译到其它目标目标。

## 编译目标的探索

之前[看了点 erlang](http://www.zenlife.tk/erlang.md)，其实就是想编译到 erlang 的字节码。但是研究之后发现，erlang 的 core lambda 部分跟 shen 的 klambda 语义差别有点大，erlang 把 pattern match 做到 cora lambda 里面了，导致我无法在只做简单的 ast 变换就把 shen 的 klambda 编译到 core erlang，进而生成 beam 的虚拟机字节码，于是这个尝试就失败了。另外，我发现在 erlang 里面完全禁止副作用，连全局变量都没有，这个约束还是挺大的。

编译到 clojure。clojure 也算是一个简洁的 lisp 方言，把 shen 编译过去实现来讲难度也并不大，社区有人实现过。然而想到 clojure 不过是运行在 jvm 之上的，就觉得考虑编译成 clojure 还不如直接考虑 jvm。

编译到 jvm，也已经有相应的项目了。研究了一番，最后放弃的原因，说到底还我对 java 并不怎么熟习，虽然 jvm 生态是没什么问题，但是平时我也用不上啊。

编译到 javascript。有三个周末的时间我确实就这么干了，并且[捣鼓了一个 demo 出来](https://github.com/tiancaiamao/cora/tree/d286c75c7c1dc4246bbf20f8a75f670bd578d5cd)。总体来说，javascript 确实是一门挺简单的语言，难怪那么多的语言都会编译成 js。在第一个周末花了不到一天的时候稍微看了一下语法，就已经能干活了，顺畅无比。中间也仅仅被 Temporal Dead Zone 给坑了一次，另外就是异步的模式有点不适应，实现 read 之类的几个 primitive 以及 repl 需要调整一下。

一个小插曲是，做编译到 js 的时候脑袋抽了，想尝试一下编译到 lua，很快放弃了这种想法。lua 并没有一个好的生态，扩展都要依赖于写 C。写 C 这种事情，写过 Go 之后再也不想做了。另外，在编译到 js 的时候我是用 trampoline 实现的尾递归，而 lua 语言本身是支持尾调用的，原以为应该这是一个很好的优势。结果发现 a and b or c，这种东西在 lua 里面居然是不能够尾调用的，坑了。

编译到 javascript 之后，总觉得 object 有点重，想看看有没什么对底层有更好控制的方式。然后 Go 的 1.11 要支持 webassembly 了。之前我就相信它是代表着未来的，webassembly 感觉会火，所以又考虑了一下要不要尝试编译到 webassembly。有人已经尝试过把 [scheme 编译到 webassembly](https://github.com/google/schism)。webassembly 是类似于一个栈虚拟机，目前还不支持 GC。编译那边只要做一个闭包变换，可以考虑用 C 之类的写 GC 和对象等 runtime 部分。本来感觉一切都挺好的，直到我思考 eval 该如何实现。webassembly 是模块级加载的，并不是动态执行 webassembly 代码，这就导致 lisp 里面的 eval 函数没法动态编译和运行。要么就再之上实现一个解释器，eval 的东西就通过解释器跑。这样子编译到 webassembly 的优势就没有了 -- 我希望获得原生的性能，然而 eval 的函数并不能。

编译到 C 在[很久以前就研究过](http://www.zenlife.tk/scheme-to-c.md)，也参考过 chicken 的做法。后来发现 chicken 的性能并不太好，所以对这个方向有些怀疑。感觉 chicken 被 cheney on the M.T.A 拖累了，它直接在栈上分配对象，会导致 GC 倾向于过于频繁。可能更多是优化得不够到位吧。

## gambit 的启发

这次我发现了 gambit。gambit 是一个编译到 C 的 scheme 语言实现。[benchmark](https://ecraven.github.io/r7rs-benchmarks/) 显示 gambit 性能仅次于 chez，排名第二！ 那它为什么跟 chicken 不一样，达到了更好的性能？我研究了一下，还是挺有收获的。

在[《The 90 minutes Scheme to C compiler》](http://churchturing.org/y/90-min-scc.pdf)里面可以看到一个大概的做法。前面部分的编译都和 chicken 一样，做 cps 变换，然后做闭包变换。gambit 在中间步骤是设计了虚拟机的！它做完闭包变换，并没有像 chicken 那样直接变成等价的 C 的代码，而是生成了它的中间层 gvm 的虚拟机指令。cps 变换使得所有的函数调用都不会返回，像 chicken 那个做法就会让栈不停增长，再用特殊的方式来处理。而在 gambit 设计的虚拟机里面，它将函数调用直接实现成 jump 了。

gambit 对 C 的使用姿势特别有趣：语言即是虚拟机。不是解释执行的，但是又不使用宿主语言本身的堆栈寄存器等进程布局，不做等价的翻译。它里面定义了各种乱七八糟的宏：

    jump: switch (pc) {
    case 0: /* (lambda () (let ((r.5 (%closure (lambda (self.10 k.6 N.1)... */
        BEGIN_CLOSURE(1,0); END_CLOSURE(1,0);
        PUSH(LOCAL(0/*r.5*/)); GLOBAL(0/*fact*/) = TOS();
        PUSH(GLOBAL(0/*fact*/));
        BEGIN_CLOSURE(2,0); END_CLOSURE(2,0);
        PUSH(INT2OBJ(5));
        BEGIN_JUMP(3); PUSH(LOCAL(2)); PUSH(LOCAL(3)); PUSH(LOCAL(4)); END_JUMP(3);
    case 2: /* (lambda (self.12 r.4) (let ((r.2 (%+ r.4 1))) (%halt r.2))) */
        PUSH(LOCAL(1/*r.4*/)); PUSH(INT2OBJ(1)); ADD();

这样子做法，性能可能比手写汇编更好。C 语言优化了这么多年，编译器自然是成熟稳定并且足够聪明的，对应的 sp, hp 和中间变量都会直接在寄存器分配，比在汇编里面手动指定更高效。另外，所有的 lambda 都丢到一个函数里面，又带来一个特别的好处， WholeProgramOptimization，即全程序优化，参见 MLton。因为所有函数都写在一起了，编译器可以不用考虑跨函数调用的 ABI，那么函数调用就可以避开过使用内存栈传参，优化有可能直接用寄存器传参。

gambit 对性能方面很有追求。网上随手搜到这篇 paper，可以体现这种态度。《Speculative Inlining of Predefined Procedures in an R5RS Scheme to C Compiler》。在 scheme 语言里面，一些基本运算像加减乘除都是函数，这些内置的函数都是可以被重载掉的，所以无法做内联。这种非常基础的操作都要走函数调用，开销还是很大的。而实际上大部分时候没人去改一些 buildin function，那么 Speculative Inlining 做的是什么呢？ 它假定了 buildin 不会被重载，先判断一下，没被改过就调用 buildin 的，否则 try 修改过的，这样前面判断的部分就可以 inline 掉了，于是避开调用开销。

    (f (car (g 5))) -> (f (let ((x (g 5)))
                          (if (and (’ ##eq? car ’ car )
                                  (’ ##pair? x))
                                  (’ ##car x)
                              (car x))))

虽然 gambit 带来很多启发，但是不一定需要使用它的做法。对于 CPS 的态度依然还是和原来一样，shen 里面并不需要实现 call/cc。CPS 引入的复杂度偏高，做完 CPS 会额外多出一堆的 closure，后期又要继续将这步变换引入的负担给“优化掉”，那么做这一步的意义是值得怀疑的。gambit 做了很重的优化，这是我不会选择做的事情。

编译目标在虚拟机这一层仍然会保留概念，但是学习 gambit 拿语言当虚拟机用。这里有个额外的好处，因为有这一层概念，会为将来解释执行和原生执行共存留下设计的空间。到时候 eval 肯定要做的，那么可以把 eval 的做成以 interpret 方式去操作这一层 vm，而其它场合用 native 方式来操作 vm。

是 C 还是 Go？ 由于 fixnum tagging 必须自己搞 GC，基本上已经是把 Go 当 C 在用了。用 C 的好处是 gcc 的 label as value 的扩展，可以做 threaded code，对比 [Go 里面 switch 实现的很糟糕](http://www.zenlife.tk/go-switch-statement.md)。坏处是用 C 写了用 Go 调用不了。我参考过[这一篇](https://blog.filippo.io/rustgo/)，但是考虑到 lisp 的 repl 是没机会重新编译的，这个方法应该用不了。为了生态还是用 Go 吧。
