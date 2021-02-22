上次 [1.0rc 版本的发布](shen-go-v1.0-rc.md)还是 19 年的时候，一晃都跨了两年了。那一次本应该是发 1.0 的，但是还想做一些代码清理和优化之类的工作，于是打了个 rc 标记。我都有一点后悔打上 rc 标记，应该当时直接打 1.0 的，这样我就不用写这一篇了（懒，现在 blog 写得少了）。

从 v1.0rc 到 v1.0 改动说大也不大，因为之前已经确定了，大的方向就是**编程语言即虚拟机**，直接将 shen 编译成 go 的代码，这是不变了的。
要说改动小呢，其实也不小，理论上从 rc 到正式发布应该都修修 bug 做做 tuning 之类的，不应该有 feature 或者大的调整，但其实这次的发版还是夹带了不少东西。所以写一写有什么看点，这个版本里面都做了些什么。

## 让 shen-go 同时支持 klambda 和 cora

最大的改动来自于这一项，让 shen-go 同时支持 klambda 和 cora。[cora](cora.md) 是我自己设计的一门 lisp 语言，在那个 repo 里面我用 C 语言实现了一个版本。C 的优势是性能好一点，可玩性更强。但是缺点是什么都要自己搞，垃圾回收要自己实现，生态也要自己搞，很不方便。所以有一阵子我脑袋一抽，决定把 cora 用 Go 实现吧。

cora 和 shen 实现起来，主要的逻辑都差不多，于是我想要不就让它们共享一份代码吧。好处维护起来方便一些，并且优化其中一个实现，另外一个实现也可以受益。
我有很多点子，有些是在 `shen-go` 那边想到，再到 `cora` 移植，又或者在 `cora` 那边先想到，然后往 `shen-go` 移植。如果都是用 Go 写的，而不是一个 Go 的一个 C 的，就可以只弄一次。

cora 和 klambda 的语义有一些不同，一个是 lisp1，一个是 lisp2；还有就是两者对 symbol 的处理，klambda 对 symbol 是不求值的。一开始，我的做法是实现一个 Evaluator 接口，然后对于 cora 和 klambda，让他们采用不一样的 Eval() 方式，其它的代码都共享。

改完之后我才意识到，其实可以基于 cora 实现 klambda，让 cora 比 klambda 更底层一点。然后 shen 编译成 klambda，klambda 编译成 cora，cora 再编译到 Go。代码生成的逻辑也可以共享一套，而不用分开维护。

把 klambda 编译成 cora 要处理几个语义不一样的地方。对于 symbol 语义的不一致，可以在 parse 阶段统一化，将 klambda 是把未绑定到 variable 的 symbol，改写成 (quote symbol)。而对于 lisp1/lisp2，可以让 symbol 映射多个 namespace，比如让 cora 使用 ns1，让 klambda 使用 ns2。

举个例子，看这个 klambda 函数：

```
(defun f ()
	(if (symbol? blah)
		...))
```

改写成 cora 之后就是：

```
(ns2-set (quote f)
	(lambda ()
		(if ((ns2-value symbol?) (quote blah))
			...)))
```

其中，符号 `blah` 被改写成了 `(quote blah)`。函数调用 `symbol?` 被替换成了通过 `(ns2-value symbol?)` 获取到符号 `symbol?` 绑定的函数，然后再按 cora 的语义处理函数调用。`defun` 改写成 `(ns2-set)`，让 klambda 使用 ns2，而 cora 使用 ns1，就相互不影响了。klambda 里面有一个 primitive 的 set 函数，让它使用 ns3。

klambda 里面还有一个 `eval-kl` 函数，这个实现方式可以先把 klambda 代码转成 cora 代码，再去 eval cora 代码实现。这一层是在 runtime 时期做的，会有一定的开销。我做完了才发现，这样的实现方式对性能的影响还挺大的。毕竟相比于直接 klambda 直接 eval 跟 klambda 到 cora 了再 eval 多了一层转换，这个转码的过程要处理 ast 树会有大量的 list 分配。但是我已经不想改回去了，无耻地把 1.0 发布拉倒。

## 用 partial evaluation 方式重新实现代码生成

shen-go 里面的代码生成的实现，原本我是转成 ANF 做的。`(* 2 (+ a 3))` 变成 ANF 之后就类似于

```
(let tmp1 (+ a 3)
	(let tmp2 (* tmp1 2)
		tmp2))
```

每一小步都会成临时变量，然后再变成 Go 代码就很容易：

```
tmp1 := builtinAdd(a, 3)
tmp2 := builtinMul(tmp1, 2)
...
```

这些临时变量在 Go 的编译优化里面都会被优化掉的，丑是丑一点，对性能倒无所谓。但是 ANF 的搞法，处理 if 之类的比较恶心。因为 Go 的 if 是 statement，而在 lisp 里面只有 expression 没有 statement。不能够 `xxx = if x { y } else {z}` 这样子。在处理这里的 workaround 的时候弄 ANF 就有一点恶心了。v1.0 这一版里面，我改成用 partial evaluation 的方式去实现了。

partial evaluation 是一个挺有意思的话题，像 java 那边的 truffle 和 graal 都是用的 partial evaluation。不过这里我说的 partial evaluation 是泛指思想。把代码“走”一遍，在遍历的时候就去做计算，同时把代码生成出来。

假设设计了虚拟机，然后实现的是一个解释器，解释器会按虚拟机的设计去做解释。只要把这个解释过程稍微改一改，不要立马解释，而是生成相应的指令，就是代码生成的过程了。生成之后的代码，重复执行多次是不会有解释的开销的。[Maru](https://piumarta.com/software/maru/) 的代码生成就是这样做的，它直接生成到 machine code，能达到 30% 的优化过的 C 的性能，考虑到代码量那么少，这是一个很了不起的成绩。

用到 partial evaluation 的思想，并不只在 code gen 这一个地方。还有一个例子是代码的简化。func 宏生成出来之后，会有许多的无用代码，比如这段：


```
(func fact
		0 => 1
		n => (* n (fact (- n 1))))
```

它会生成 

```
(set (quote fact)
     (lambda (p40)
       ((lambda (cc41)
	  (if (if (cons? (cons p40 ()))
		  (if (not (null? (cons p40 ())))
		      true
		    false)
		false)
	      (if (= 0 (car (cons p40 ())))
		  (if (null? (cdr (cons p40 ())))
		      1
		    (cc41))
		(cc41))
	    (cc41)))
	(lambda ()
	  ((lambda (cc42)
	     (if (if (cons? (cons p40 ()))
		     (if (not (null? (cons p40 ())))
			 true
		       false)
		   false)
		 ((lambda (n)
		    (if (null? (cdr (cons p40 ())))
			(* n (fact (- n 1))) (cc42)))
		  (car (cons p40 ()))) (cc42)))
	   (lambda () (error "no match-help found!")))))))
```

这里面的 `(cons? (cons p40 ()))` 就可以直接变成 true，而 `(if true x y)` 又可以继续再简化成 x ... 经过简化之后，可以变成下面这样的代码：

    
```
(set (quote fact)
		(lambda (p50)
		((lambda (cc51)
			(if (= 0 p50)
				1
			(cc51)))
		(lambda ()
			((lambda (cc52)
				((lambda (n)
				(* n (fact (- n 1))))
				p50))
			(lambda ()
				(error "no match-help found!")))))))
```

那么这个简化过程怎么实现的呢？也算是一种 partial evaluation！

## 性能优化相关

优化都是一些小优化，没有上面的那些改动大。上面的改动都伤筋动骨的，一改代码全要影响。

### 优化 integer 的表示

在 C 语言里面，可以玩 fixnum tagging 这样的 trick: 31/63 位的 fixnum。因为对齐的原因，低3位肯定是全0的，可以拿最低位作为标记位，区分是一个整数，还是一个指针。但是 Go 不能这么玩，因为如果使用 uintptr 就不能保护对象不被 GC 掉了。

如果不做优化，每次 `MakeInteger(42)` 就要分配一个堆上的对象了。这样对于涉及数值计算的性能就会特别差。我原本一度以为 [fixnum 的优化](shen-go-next-notes.md)在 Go 里面是做不了的，后来想到了一个方法。

`MakeInteger()` 仍然返回一个 `unsafe.Pointer`，只要这个 pointer 不是指向无效地址的就行。那么其实可以把比如 0-1M 这么多的 integer 预分配出来，也只浪费了 8M 的空间，这样再生成比较小的数字的时候就直接返回，以空间换时间。

后来无意中看到，在 [Go 1.5 版本](https://golang.org/doc/go1.15#runtime)里面，也做了这样的以空间换时间的优化。

> Converting a small integer value into an interface value no longer causes allocation.

我自己想到这一点，比我知道 Go 里面用了这样的技巧，要早那么一点，所以心里还有一点小小的得意呢： Go 里面的这个优化，是我在 shen-go 早就玩过了的。

### 优化 symbol 的表示

symbol 原来是用数组内的 offset 表示的，所有的 symbol 都放在一个大的 array 里面。array 是一块连续的内存空间，所以在 symbol 对象非常多的时候，realloc 其实会有一些潜在的性能问题。v1.0 这次做了一个小的调整，就直接用 symbol 对象的地址了。

### 优化 closure 的表示

cora 的闭包表示是可视化的，借鉴 femtolisp 的做法，直接用 lambda 的 sexp 表示：

```
(lambda (a) 3 (b . 5) (c . 7)) ;; 在 ((b . 5) (c . 7)) 环境下面的闭包
```

因为 klambda 现在编译到 cora，这次我把 closure 的表示也改了一个，从 struct 的 env + body 改成了 sexp 的形式。这会让调试起来方法一些。

### 优化 ControlFlow 的表示

在 Eval 的时候，需要

```
func Eval() {
    var ctx ControlFlow
	trampoline(&ctx, ...)
}
```

这在 C 语言里面 ctx 在栈上分配，没有开销，然而在 Go 里面，会 escape 到堆上分配。eval 是非常频繁的调用，我不希望每次调用都有分配。所以优化了一下 ControlFlow 的表示，callee 直接复用 caller 的 ctx。这个复用需要“记住”并恢复之前 caller 的 data 位置。

```
func Eval(ctx *ControlFlow) {
    trampoline(&ctx, ...)
}
```

### 优化函数调用

函数调用如果走这样的模式，是比较低效的：

```
str := symbolGet(sym)
fn := functionTable[str]
fn()
```

尤其是由 klambda 翻译成 cora，还需要 `((ns2-value sym) ...)` 要先调用一次 `ns2-value`，变成 Go 代码就是：

```
str := symbolGet("ns2-value")
fn := functionTable[str]
fn1 = Call(fn)
...
ret = Call(fn1, ...)
...
```

对于 primitive 调用，这些全部可以 inline 掉，直接变成

```
builtinIsSymbol(...)
```

关于性能优化这块的数据，在优化 ControlFlow 的表示之后，跑测试的时间从 12s 多下降到了 7s 多，然后优化完函数调用之后，达到了 5.54s。这是到目前为止获得的最好的数据，后来又性能回退了，最终现在的版本是 9.X 到 10s 的样子。

为什么回退了呢？我发现瓶颈是在 evalKL。前面 5.54s 的实现是定义了 `Evaluator` 接口，分别为 klambda 和 cora 实现了 `Eval`。这里只有接口表示这样一点点开销，应该不高。而最新的实现是改成了将 klambda 转义成 cora，这个步骤开销比较大。

java 版本的 shen 实现是 4.X 秒，它是走 jit 的。而 shen-go 如果往追求性能方向优化，我觉得 5s 内是没问题的，应该可以跟 java 版本的性能在同一水平。由于现在的 cora 有 Go 和 C 两个版本的实现，我顺手 benchmark 对比了一下，果然生成到 Go 对比生成到 C，性能被秒到渣渣都不剩。既然都用 Go 写了，也就别想着把性能追求放到第一位了。

## 其它杂项

编译升级到最新的 Go 1.16。这几天 Go 1.16 刚刚发布不久，它有一个新的 feature 是可以 embed 数据到 binary 里面，正好可以利用这个 feature 把初始化的文件 `init.cora` 给打包进 binary。

把 shen 的代码升级到了最新的 22.3，虽然说是最新的，但最新版本[官方的 repo](https://github.com/Shen-Language/shen-sources) 距上一次更新差不多都有一年了，小众的项目不太活跃啊。
