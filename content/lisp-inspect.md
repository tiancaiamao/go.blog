在写 scheme/shen/cora 等等这一类 lisp 代码的时候，一般我是通过 repl 来调试的。无状态的表达式，在 repl 环境里面直接 debug 足够简单。
我没有用 lisp 写过什么大型的程序，在其它语言中，比如 C 和 Go，有些场景我会视情况上单步调试，使用 gdb 和 delve 这类调试器。

在上次发现了 [tailification](tailfication-delimited-continuation.md) 之后，又准备把 cora 改成编译到 C 的方式了，代码怎样调试便成了一个问题：编译到 C 的实现方式是不再具备那么方便的 repl 的。

想了一想，觉得我可以用 cora 实现一个解释器，这个解释器里面不用太考虑性能的事情，主要就是有一个 repl 环境。顺便，只需要简单 hack 一下这个解释器，还可以将它变成一个调试器，具备基本的"断点调试"功能。

## 下断点

想象我们在写 C 的时候，如果不使用 gdb，如何将代码停下来调试? 我们可以在需要的地方，scanf 让程序停下来，可以接受一些简单的命令来打印一些变量的值，这就是最简单的下"断点"的原型了。代码执行到某处之后，停下来，inspect 此处的一些信息。在 C 语言里面 inspect 不太好弄，但如果是在自己写的一个解释器里面，inspect 是比较简单的。

我们可以在解释器中实现一个特殊的原语叫 inspect，然后在我们代码需要下断点的位置，调用 (inspect)。执行到 inspect 之后，代码会进入到 debug 的 repl 里面。相当于先 scanf 一下，读到 debug 命令后，决定如何 eval 它。

## debug 的 repl

```
(func eval
	['inspect] env k => (inspect env k)
	...)
```

在 inspect 函数里面，我们可以检查 read 的 sexp，对于非特殊命令，我们直接使用原始的 eval 方式，这个 eval 可以做任意想做的计算的。

```
(eval '(do (set 'fact (lambda (n)
			(if (= n 0)
				1
				(do (inspect)    ;; breakpoint here
					(* n (fact (- n 1)))))))
			(fact 5)) () id ())
```
			
```			
debug> 1
1
debug> 2
2
debug> 5
5
debug> (+ 3 2)
5
```

还可以读到断点位置的变量的值，inspect 这个时刻的 env 我们是传给 debug 里面的 eval 的：

```
debug> n
5
```

## 恢复执行

在 debug repl 里面，如果读到的 sexp 是一些特殊命令，我们就特殊处理，比如说我们可以 `(continue xx)`:

```
debug> (continue 1)
debug> n
4
debug> (continue 1)
debug> n
3
```

continue 的时候我们传了一个值，这个值是什么意思呢？ 它就是传一个值给 inspect 位置的 continuation，恢复从那里的继续执行。看到没有，一不小心，就发明了 call/cc！ 只要我们用 cps 的写法去实现解释器，相应的 call/cc 其实是非常容易实现的。只要把解释器的那个 k，作为一个值传到被解释的代码去就行了。

在 debug 的 repl 里面，如果是非 continue 命令我们的 eval 就传 id 作为 eval 的连续参数，如果是 continue 命令，我们就传前面的 k，就恢复之前的执行了。

对于像 `(+ (inspect) 1)` 这样的输入，假设我们在 inspect 进入 debug repl 之后，使用 `(continue 5)`，这一个表达式就会计算得到 6。 

## 调用栈

除了断点，打印断点位置有一些变量信息，调试的时候还有一个很有用的信息是调用栈。如何实现调用栈功能呢? 我们可以把这个信息也传到解释器的 eval 里面。每次执行函数调用的时候，就将函数信息进栈。

```
(func eval
	[f . args] env k stacks =>  (eval f env (lambda (f1)
										(eval-list [] args env
											(lambda (vargs)
												(eval f1 nenv k (cons f stacks)))))
										stacks)
	...)
```

这样在我们的 inspect 的 repl 里面，可以新加一个 `(stack)` 命令用于打印栈信息：

```
debug> (stack)
(fact fact)
debug> (continue 1)
debug> (stack)
(fact fact fact)
debug> (continue 1)
debug> (stack)
(fact fact fact fact)
debug>
```

这里展示比较粗糙，直接用链表存着了，可以优化一下显示。如果出现递归调用怎么处理呢? 我们可以检测一下递归，当前 f 跟 stacks 最近的内容重复多少次之后，就不再加入 stacks 了，打印的时候也可以改成用  `...` 省略号来表示。毕竟在 lisp 里面递归是十分常见的。

## delimited continuation 实现

在 cora 中一个很重要的东西是 [resumable exception](resumable-exception.md)，或者说 delimited continuation。在解释器里面怎么实现呢?

相应的语法是 `(try (lambda () exp)  (lambda (res resume) ...))`。在 exp 里面可以 `(throw xxx)`，throw 之后会跳转到 catch 的逻辑里面，而 throw 时刻的 continuation 则作为参数传到 resume。

所以 eval 在解释前面 exp 的 thunk 的时候，它是有两种可能的 continuation 的，一种是正常返回，另一种是跳转到异常处理逻辑。我们可以弄一个全局变量，用栈来保存异常处理分支的那个 continuation，这样 throw 的处理就很直观了： 


```
(func eval
      ['throw x] env k => (let k1 (.pop-k)
								(eval x env (lambda (vx)
										(k1 vx k))))
	  ...)
```

在 try 的时候，需要将异常处理的逻辑进栈保存：

```
(func eval
      ['try exp ['lambda params body]] env k =>
						(begin
							(.save-k (lambda (v resume)
								(let nenv (.env-extend params [v resume] env)
									(eval body nenv k))))
							(eval (cons exp ()) env k))
	  ...)
```

想明白了还是挺简单的，毕竟是用 cps 解释器形式来写的，获取 continuation 变得特别容易了，整个解释器的[代码也就几十行](https://github.com/tiancaiamao/cora/blob/38c0125798254732e51fbc05a402f88bc59b4b8f/lib/inspect.cora#L54-L78)。

等我把 compile to c 弄好后，再把这个用 cora 写的 interpreter + debugger 编译出来，很容易就能弄好 repl 环境，然后编译和解释执行应该就都 OK 了。
