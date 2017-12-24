想不到居然已经时隔一个多月了，[接上篇](klambda.md)，编译到字节码的工作算是基本完成。[shen-go](https://github.com/tiancaiamao/shen-go)是shen语言在Go的实现。前几天给shen-go打上了v0.1的tag。不知道多少天熬夜，连续好多周末宅着调试代码，看到所有的测试都跑通那一刻，真是感觉所有的努力都没有白费。

好的代码是艺术品，功能确定好，反复打磨，当一行代码也不能再增加，并且一行代码也不能减少的时候，就比较接近于完美了。其中[有50行我是比较满意的](https://github.com/tiancaiamao/shen-go/blob/16ae75533739e9138c8eaa914beb3cc0ae3d3753/compiler/compile.shen)，称得上可以拿出手的作品。

50行代码的编译器！就像王垠的40行一样，大多数的人不会看得懂它干了什么，嗯，所以编译器对他们永远是魔法。

其实还可以更短，真正也就40行，这么短的代码里，实现尾递归优化，支持了异常机制，还做了自动curry化！

编译出来字节码是[长这样子的](https://github.com/tiancaiamao/shen-go/blob/16ae75533739e9138c8eaa914beb3cc0ae3d3753/bytecode/compile.bc)，很奇怪是不是？偷懒直接用sexp表示的。

事情总算可以告一段落，接下来是写一些可能的优化方向。

JIT 是肯定不做的，坑太深了。一个很重要的点就是克制自己什么东西都想弄一弄，明确哪些feature是重要的，不然技术的研究就没完没了了。其实做的过程中，好几次差点没克制自己去写一个C语言的版本，光想想涉及GC就没完没了。(如果哪一天真用 C 重写了，那它应该放到 cora 那个项目里面了，神圣的代码仓库里面只能有 C 和 lisp。嗯，“每个lisper都应该拥有属于自己的lisp”)

纯的 evaluator 执行效率太低了，bytecode 是一个比较好的折中的平衡点，投入的精力和达到的效果。在这个大的取舍之下，考虑的就是三部分：

* 编译器优化
* 虚拟机优化
* 运行时优化

虚拟机优化方面， [zinc 的PPT](https://xavierleroy.org/talks/zam-kazam05.pdf)里面讲到的大部分，差不多都优化到头了。最新的master正在拆环境，静态部分和动态部分。环境和调用栈合并没做，编译期决定变量位置没做。都是在考虑了代码引入的复杂程度，和带来的性能提升上面，做的取舍。

## peephole optimizations

加速特定的函数来实现优化效果。由于 shen 到 klambda 的编译是用 shen 实现的，也就是会跑 bytecode。里面的 hash 等等常用函数，如果用原生实现而不是跑 bytecode，这块应该是可以有不少收益的。

涉及到的主要包括 hash，dict。然后还有文件IO相关的，都可以改用原生实现去替换掉 shen 编译器里面的 bytecode 实现。还有一些基本的函数。它里面有些函数的实现效率实在让人看不下去，比如 [symbol?](https://github.com/tiancaiamao/shen-go/blob/16ae75533739e9138c8eaa914beb3cc0ae3d3753/ShenOSKernel-20.1/sources/sys.shen#L163)

## fixnum tagging

目前在 shen-go 里面，所有东西都是用一个 Obj 表示的，是一个boxed value。它实际上是一个指针，指向一块内存。那里面将类型和值打包到一块了。
数字类型，现在直接用的 float64。也是开始做的时候深思熟虑后的取舍：float64 其实表示定点数其实可以是"准确"的，根据IEEE 754标准，只要算出阶码部分，再看后面多少位尾数为0，可以确定它是否真的有小数部分。

那么 fixnum tagging 又是要优化什么呢？是这样的，假设内存是对齐的，由于 Obj 都是指针，最低3位必然全是0。如果用最低一位为1当作区分，我们可以剩下63位用来存储 fixnum。这样子，最后1位为0它就是一个数字，否则它是一个指针。数字类型就可以不用在堆上分配空间了。

在 C 里面是司空见惯的技巧，尚不确定 Go 里面能不能做，Go是托管内存的，乱玩也许会 panic。

## threaded code

[Threaded code](https://en.wikipedia.org/wiki/Threaded_code)又是一项在 C 里面非常常用的技巧。准确来说是 gcc，编译器提供了一个扩展，可以直接拿到标签对应的机器指令的地址，于是可以直接jmp过去，让指令预测更友好，相对于 switch-case 的写法要更加高效。

    switch instruction {
      case Push:
      case Pop:
    }
    
很不幸，这个 Go 语言里面似乎搞不了。

    thread:
      &pushA
      &pushB
      &add
      ...
    pushA:
      *sp++ = A
      jump *ip++
    pushB:
      *sp++ = B
      jump *ip++
    add:
      addend = *--sp
      *sp++ = *--sp + addend
      jump *ip++

## 优化symbol表示

对于 lisp 语言，优化 symbol 还是很有必要的。目前我还是用 string 来实现的，而实际上 symbol 应该就类似于指针，symbol 做 eq 比较应该是没任何开销的。

可以做一个 trie 树结构做去重，存储开销比 hash 低，创建 symbol 的时候先查。每个 symbol 直接在一个数组上面分配，返回在数组的位置记录到 trie 树结构里面。这样对每个 symbol 就有唯一的对象了，也不用逐个字符比较。

## 优化函数调用

现在的函数全部是放在一个哈希表里面的，好处很明显：实现很简单，像递归函数完全不用特别对待；可以处理将编译期和运行期分离，比如：

    (defun f (X) (g X))
    
函数f的定义里面引用到了g，g是可以编译期不用知道的，也不用预先声明。

缺点是，函数调用其实是运行期去查 hash 表的。前面既然说了 symbol 优化，symbol 全局唯一，可以被优化成指针。如果我们在 symbol 结构里面额外绑定一个函数的指针，就可以避开函数调用的时候去查 hash 表了。调用过程也就从 hash 查表过程直接变成了取结构体里面的field。

## 优化let编译

好吧，我承认我偷懒了，let 我是转是 lambda 去做了。

    (let X 3
        (let Y 5
            X))
            
转换出去是

    ((lambda X
        ((lambda Y
            X) 5)) 3)

这样做出来有个问题：注意在里层的lambda中的 X 是闭包的，所以会有在堆上额外的分配。如果直接做 let 编译，这里就可以做到栈上分配了。说起来还有尾递归优化那边，我得检查一下代码，应该也还有提升的空间。

## const常量去重

跟 symbol 类似。

......
