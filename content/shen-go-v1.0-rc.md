时隔一年半，终于发布 shen-go 的下一个版本，[念念叨叨了好久](/shen-go-next-notes.md)。可喜的是，这次发布的是 1.0 RC 版本！为什么从 0.1 0.2 0.3 直接跳到 1.0 了呢？因为我认为这次使用的实现方式，应该是这个项目的最终形态了：我把 shen 代码编译成 Go 代码！
那么为什么是 RC 呢？因为在 GA 前可能还会做一点清理和打磨，而目前忙碌的程度，不一定有空赶在年底 Release。不过 1.0 RC 我已经非常满意，所以就先放出来了。


## 编程语言即虚拟机

许多次提到这个概念了。把一个编程语言，直接当成编译的 target，而不是定制一个虚拟机，优化虚拟机，优化 runtime 等等。

这其实是一个非常 fancy 的概念，让我们想一想，假设我发明了一门新的语言，为了获得良好的性能，需要将这门语言编译成二进制，这是常规的实现模式。其实很难，要学习编译器的知识，不断练习做一个又一个小玩具，还有语言设计方面的知识点，积累个 5 6 年的经验大概开始收放自如。

假设我们不这么做，假设我们手上已经有一门可以编译到 native 的低级语言，比如说 C 或者 Go。然后我们可以先为自己的语言实现一个解释器，写解释器其实不难的。然后我们就可以用我们设计的语言写代码了，嗯，低级语言使用还是很痛苦的，所以我们才捣腾自己的语言嘛。

接下来，实现一个自己的语言到 target 语言的代码的编译。有了这么一个东西之后，其实通过 新语言 => target 语言 => 二进制，我们就可以让自己实现的语言，获得原生性能了。

我们做了什么？其实完全不需要复杂的编译器知识。只是写了一个新语言的解释器，以及一个新语言编译成 target 语言的转换工具。

写解释器，得原生性能！

## 解释和编译的完美融合

上面说的新语言当然是 shen，而上面所指的 target 语言，也就是 Go 了。有了 shen 解释器，我们就可以以解释模式执行 shen 语言，而有了 shen 转换成 Go，就可以获得原生的性能。最后还差一步，融合！

shen 是需要 REPL 支持的，所以解释模式不能丢。而如果能让解释模式，和编译后的代码共存，就完美了。正好，Go 是支持 plugin 的。于是可以这样干：把 shen 代码编译成 Go，再生成插件，再 load 回解释器里面。
融合模式下，如果某个函数，有 native 实现的，就调用 native 的实现，如果没有，则使用解释实现。

这里面有一些细节，解释模式下，函数就是代码加环境的数据结构。而编译模式下，函数就是使用函数指针了。最要紧的，是如何互调用！解释模式调用函数指针不难，跟 primitive 或者说 builtin 的都是类似。反之，原生如何调用解释模式呢？需要把 closure 结构封装好，并且对 Call 方法也封装好，生成出来的代码都是 Call 一个 callable 的东西。解释模式下的闭包，或是编译模式下的函数指针，都是 callable 的，这样就解决了。

## 如何实现代码翻译

代码生成阶段，其实跟[前面的博客](/language-as-vm.md)写的不太一样了。既没有做闭包变换，也没有做 CPS。就非常直白的翻译成 Go。

比如说嵌套的表达式， (f (g a) 1) ，会这样子展开：

```
reg0 := a
reg1 := g(a)
reg2 := 1;
reg3 := f(reg1, reg2)
```

反正就是把中间结果全部引入临时变量。

对于 if，由于 shen 语言里面是 expression，而在 Go 语言里面 if 是 statement，需要这样用声明和赋值：

```
(if x y z)
```

```
reg0 := x
var reg1 Obj
if reg0 {
    reg1 = y
} else {
    reg1 = z
}
```

函数的话，shen 的函数对应到 Go 的函数：


```
(defun f (x) (+ x 1))

```

```
MakeNative(func(__e *Evaluator, __ctx *ControlFlow, __args ...Obj) {
    x := args[0]
    reg0 := 1
    return PrimAdd(x, reg0)
}, 1)
```

函数调用协议，有尾递归和非尾递归的。非尾递归就生成 `Call(xxx)` 的，会使栈增长。

## 如何编译尾递归

shen 语言要求严格尾递归。这个特性的实现用的是 trampoline 技术


```
type ControlFlow struct {
    kind int
    f Obj
    args Obj
}

func trampoline() {
    for ctx.kind != ControlFlowReturn  {
        f(ctx)
    }
}

func f(ctx *ControlFlow) {
    // ... 尾递归的时候，设置好类型，然后 return
    // 下一轮在 trampoline 循环里面会继续调用，而调用 f 不会使栈增长
    ctx.kind = ControlFlowApply
    ctx.f = f
    ctx.args = args
    return
}
```



## 如何编译 partial apply

shen 要求支持 partial apply。在生成的 Go 代码里面，我使用的是变参函数声明，同时把需要的参数个数也记录下来。

```
type scmNative struct {
    fn func(ctx *ControlFlow, args ...Obj) // 函数指针
    captured []Obj // partial apply 产生的闭包
    require int  // 参数个数
}
```

包装一个 Call 函数，如果参数刚好够，就调用。否则，生成闭包：

```
func Call(f, args ...Obj) {
    if len(f.captured) + len(args) == f.required {
        return f.fn( ...)
    }
    
    f.captured = append(f.captured, args)
    return f
}
```

## 如何实现 trap-error

shen 的 trap-error 是用来支持异常处理的 `(trap-error expr handle)`。执行 expr，如果出错了则调用 handle 处理错误。类似于 try catch。

在 shen 编译成 Go 的时候使用了 defer panic 来实现。稍微封装了一下。

```
type tryResult struct { e Obj }

func Try(exp func()) tryResult {
    defer func() {
        if e := recover(); e != nil {
            return tryResult{e}
        }
    }()
    // exp
}

func (tryResult) Catch(func (e Obj)) {
}
```

然后就可以这样用了：

```
Try(func() {
    // 生成的 exp 的代码
}).Catch(func() {
    // 生成的 handle 的代码
})
```

## 后续工作

时间有限，这个版本中暂时没有做 tuning 相关的工作。

粗略地看，没有想象中那么大的提升。使用纯解释模型的时候，跑测试集大概在 110s 的样子。而当我把 shen 的编译器部分的代码全部 native 化之后，再去跑测试集大概在 14s 的样子。
在上一个版本 0.3 基于 VM 的实现，性能有回退大概也在这样量级，之前 0.2 基于 VM 的实现，应试在 12s 以内，反而是 VM 优于当前 native 版本的。

不过现在的测试是使用的 load 代码的方式测试的，如果把测试代码也给编译出来，估计结果会快不少。如果能走到 native 而不是解释方式，猜测进到 10s 应该是没什么难度的。然而，从整体上看，
最终 shen-go 这个 port 还是达不到像 JVM 版本走 JIT 的性能，4s 级别。估计至少要优化对象表示之类的事情。

在发布 1.0 GA 之前，大概会清理一下现在的代码，赶出来的代码比较乱了。

然后应该会升级到最新的 [shen release](https://github.com/Shen-Language/shen-sources/releases) 吧，在新版本里面 @tizoc 做了不少工作。

旧的 VM 的方式估计也可以干掉了。解释加编译的 fusion，从性能，代码简单，灵活性，可维护等等各方面都比 VM 方式要好。

再有就是性能的 tuning 了，不多说。
