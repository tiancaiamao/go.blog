接[上篇](algebraic-effect.md)，之前想的场景太简单。然后继续研究了好久，才终于实现了 algebraic effect。

## algebraic effect 的实现方式介绍

总体来说，algebraic effect 的实现方式有好几种，大概可以分为以下几类。

1. 通过修改 runtime 的

典型的比如之前提到的 koka 的 [libmprompt](https://github.com/koka-lang/libmprompt) 的库的做法，以及 ocaml 的实现也是改 runtime 的。
这种实现方式，都依赖于捕获运行时栈，然后包装成一个 continuation。其中栈的保存和恢复，要么基于汇编搞，要么一些 setjump 等一些特殊的机制。然后栈的大小就会遇到跟 Go 一样的问题，要不用分段栈等机制去优化。

2. 基于 delimited continuation 的方式

比如 koka 的这一篇 [Generalized Evidence Passing for Effect Handlers](https://www.microsoft.com/en-us/research/uploads/prod/2021/08/genev-icfp21.pdf) 它是讲优化的，实际上也是 delimited continuation 的实现形式。
算是做了局部 CPS 变换。

3. Free monad

关门，放 [oleg](http://okmij.org/ftp/Computation/free-monad.html)

free monad 这种方式实现 algebraic effect 的优化是对语言完成无侵入，可以做成库的形式。缺点是...额，理解 monad 容易造成不可逆的脑损伤。
之前读过一篇很好的博客，自己终于把 Free monad 在做什么大概的理解了，不过现在已经忘记是哪篇博客了。说说理解，本质就是 effect 和 handle 那些东西，变成了编码，然后外层的语言，是变成了一个解释器，来驱动编码的执行。
通过 monad 表示，effect 的代码被变成了数据结构了，free monad 的执行过程就是解释和驱动数据结构变化的过程。

4. 基于 yield/generator 等野路子

上面这些 paper 的都太理论了，数学符号看得人头晕。其实有些野路子的实现，似乎对智商更加友好一些。

典型的比如这个系列的 [Algebraic Effects in JavaScript](https://dev.to/yelouafi/algebraic-effects-in-javascript-part-1---continuations-and-control-transfer-3g88) 写得很不错。它里面是通过 generator 来切开函数栈，用链表把栈再链起来，如果当前栈里面没有 effect 对应的 handle 就沿着链表往上一层的函数去寻找 effect handler。

最后是这一篇 paper [One-shot Algebraic Effects as Coroutines](http://logic.cs.tsukuba.ac.jp/~sat/pdf/tfp2020-postsymposium.pdf)，可以说本文其实就是基于它的实现。

## yield 的实现

lua 里面原生就支持 coroutine，在 javascript 里面，支持 generator，算是乞丐版的 coroutine。而我自己发明的 lisp 语言里面，连 yield 都没有，这就很着急。

好在 lisp 的表达力很强大，实现 javascript 那种弱鸡的 yield 还挺容易的。为什么说 javascript 的 yield 很弱鸡呢？ **因为 yield 不能够在任意位置，只能够在定义为 `function*` 的函数里面！**

比如说这里：

```
function* f(){
   g()
}
```

不能够在函数 f 调用 g，然后在 g 里面去 yield，只能够在函数 f 里面　yield。

实现这么弱鸡的 yield，在 lisp 里面写个简单的宏就可以了：

```
(yield v (do some thing)
	;; continue with result v)
```

yield 这个宏可以将 (do some thing) 的结果传出去，然后暂放弃当前函数的执行。等调用者再次调用时，会传一个值 v 到当前 yield 出去的位置，接收到这个值之后继续往下执行。

所以其实就等价于返回两个值出去，一个是需要 yield 的结果 (do some thing)，另一个是剩下的计算，也就是收到 v 之后会继续执行的内容，剩下的计算就可以用函数表示。
没有多值返回的时候，需要包装一下：

```
;; input: (yield v e1 e2)
;; output: ['Next e1 (lambda (v) e2)]

(defmacro yield (exp)
  (let v (cadr exp)
       e1 (caddr exp)
       e2 (cdddr exp)
       ['make-yield-next e1 ['lambda [v] . e2]]))
	   
(defun make-yield-next (v k)
  ['Next v k])
```

使用上也很方便：

```
(defun f ()
  (yield v1 42
	;; 返回一个 42，等待调用都处理后，传入参数 v1 继续往下执行

	(yield v2 (+ v1 1)
		;; 返回 v1+1，再次等待调用者处理，传入一个 v2 后继续往下执行
		v2)))
```

是不是很像 let 呢？

```
(let v1 42
	(let v2 (+ v1 1)
		v2))
```

区别只是 yield 是放弃了当前执行栈，直到再次被调用的时候，才会得到 v1 v2 的。

```
;; handle-yield-result 对 generator 返回结果处理，如果不是最终结果，则调用 handle 处理返回的值，处理后继续执行 generator
(func handle-yield-result
	['Next v cc] handle => (cc (handle v))
	res _ => res)
	
(handle-yield-result
	(handle-yield-result (f)
		(lambda (x) x))
		   (lambda (x) x))  ;; 计算结果得到 43
```

简单的几行写个宏，就在 lisp 里面实现了 javascript 的 generator，性价比极高。
语法上面的区别是，在 javascript 里面是用 `generator.next()` 驱动的，原地修改 generator。
而这里返回了值，同时也返回了 generator，用 `handle-yield-result` 来驱动。

## 函数染色问题

在 javascript 的世界里，异步编程的标准做法是 async/await。其实就是 yield 一个 promise 的语法糖。promise 其实还是一种 monad。
这种异步并发，有一个问题是"函数染色"，解释就是所有使用了 await 的函数，都需要添加上 async 的函数签名。
一层套一层，最下面一层函数调用了异步 io 加了 async，调用它的函数也必须加下 async，再到上层的调用者，污染整条链路。

如果一门新语言，从标准库到上层，都是全 async 的还好一点，但是对于有些历史包袱的语言，有些库已经是非 async 的，这个染色的处理就很难受。

monad 也是一样的坏毛病，也是会染色。所有返回的值不再是一个普通的值了，而是一个 monad，需要用 bind 函数解出来。一层一层往上染色，整个调用链路都变成 monad 的。

这也是为什么要用 algebraic effect，而不是用 monad 这套。algebraic effect 没有函数染色的问题，所以是“可组合”的，而 monad 不是可组合的。

我记得在哪里读到，说 **"algebraic effect 相对于 delimited continuation 的关系，就好比 if else switch 相对于 goto"**...

## 通过 yield 实现 algebraic effect

代码里面抛出 effect，这个是用 yield 来实现的。

```
(yield v (eff 'XX val)
	v)
```

这样就是抛出一个 'XX 的 effect，然后由 effect handler 去处理，处理好之后，再处理结果的值传回来，就是这儿 `v`，然后继续往下执行。

整个 API 对外的接口主要是 handler，它需要 effct handlers，形成一个可以处理 effect 的逻辑。
其中参数 th 是 thunk 的意思，是一串代码，也就是待处理的，可能抛出 effect 的逻辑;
参数 effhs 是 effect handlers，这里以 `[eff handler ...]` 这样一个列表的形式组织。一个 effect　对应一个 handler；
参数 vh 是，如果 thunk 的执行得到最终结果，而不是一个 yield 的 effect，则用将结果返回给 vh；

```
(defun handler (vh effhs th)
  (handle-yield-result vh effhs (th ())))

(func handle-yield-result
      vh handlers ['Next resend remain] => (handle vh handlers resend remain)
      vh handlers res => (vh res))
```

也就是说，effhs 是用于处理 thunk 执行中 yield 一个 effect 的情况，会调用相应的 effect 的 handler，而 vh 用于处理 thunk 返回最终结果的情况。

我们可以最终提供一个 `(with-handle eff th)` 这样的宏包装一下：

```
(with-handler ['C1 effh1]
	     (yield v2 (eff 'C1 42)
		    v2))
```

但它其实是 handler 函数调用的简写：

```
(handler (lambda (x) x)
	 ['C1 effh1]
	 (lambda (_)
	   (yield v2 (eff 'C1 42)
		  v2)))
```

看一个使用的例子：

```
(defun effh1 (lambda (v k) (k v)))

(handler (lambda (x) x)
	['C1 effh1]
	(lambda (_)
		(yield res (eff 'C1 10)
			res)))
```

thunk 里面，会 yield 一个 C1 的 effect，然后 handler 会从自己的 effect handlers 列表里面查找对应的 handler，如果找到了，就执行。

真正的核心是这样一个函数：

```
(func handle
      vh handlers ['Eff e v] remain => (let effh (find-effect-handler e handlers)
					    k (lambda (arg) (handle-yield-result vh handlers (remain arg)))
					    (if (null? effh)
						(yield res ['Resend ['Eff e v] k]
						       res)
					      (effh v k)))
      vh handlers ['Resend ['Eff e v] k1] k => (let effh (find-effect-handler e handlers)
						    (if (null? effh)
							(yield res ['Resend ['Eff e v] (rehandle k handlers k1)]
							       res)
						      (effh v (rehandle k handlers k1)))))
```

然后 `['Resend ['Eff e v] ...]` 这个是什么鬼？ handler 里面有自己能处理的 effect handler，如果遇到不能处理的呢？就直接往上层抛出去，所以就有了这个 Resend。
也就是说，handle 遇到 Resend 的时候，是它下面的一层处理不了某个 effect，抛出来的。这一层的 handle 需要检查自己是否能处理这个 Resend 的 effect。如果它能处理了，就用相应的 handler 去处理。
也就是这一行：

```
(effh v (rehandle k handlers k1))
```

处理之后返回到哪儿？ 这里面有个 rehandler，它是这样的继续之前的未完成的计算 `(k arg)`，但是是在一个加上了当前 effect handler 的环境里面：

```
(defun rehandle (vh handlers k arg)
  (handler vh handlers (lambda (_)
                         (k arg))))
```

因为有可能执行过程中有可能再次抛出 effect，所以需要重新加上 effect handler。

最后一个完整的例子

```
(with-handler ['C3 effh3]
(with-handler ['C2 effh2 'C1 effh1]
	(yield a (eff 'C3 10)
		(yield b (eff 'C3 13)
			(yield c (eff 'C3 17)
				(+ (+ a b) c))))))
```

以及最后的实现，其实就不到50行代码，见[这里](https://github.com/tiancaiamao/cora/blob/a9406c82b23a8cde367f8d2d0614ae6ec1c23979/lib/eff.cora#L1-L42)。
