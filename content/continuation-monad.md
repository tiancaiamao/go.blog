monad 这东西，懂了很简单，向不懂的人解释起来又很难，网上有不少的教程。这个不是 yet another monad tutorial。

其实在思考 yield 之类的问题没参透，阴差阳错地把 continuation monad 理解了，于是记一下笔记。

monad 里面有 return 和 bind ，它们的类型分别是

```
return: a -> M a
bind m f: Ma -> ((a Mb) -> Mb)
```

然后 continuation monad 的 return 和 bind 的定义：

```
(defun return (x) (lambda (k) (k x)))
(defun bind (m f) (lambda (k)
		    (m (lambda (res)
			 ((f res) k)))))
```

我们先写一个 CPS 版本的 fact 函数：

```
(defun fact (acc n k)
  (if (= n 0)
      (k acc)
    (fact (* acc n) (- n 1) k)))
```

然后做 curry，把 k 写到返回值里面：

```
(defun fact (acc n)
  (lambda (k)
    (if (= n 0)
		(k acc)
      (fact (* acc n) (- n 1) k))))
```

那么 `(fact 1 5)` 是什么呢？ **其实它就是一个 continuation monad！** 所以 continuation monad 就是一个 `(lambda (k) ...)` 这样的东西。
这玩意是个啥呀？**它接受一个连续，返回最终的结果**，可以把它的类型记作：`Cont -> r`。

至于 continuation 又是什么，continuation 就是剩下的计算 ... 一般就是这种形式 `(lambda (val) ...`，它收到一个值之后会继续计算。
比如最典型的 `(defun id (lambda (val) val))`，这个东西就是一个 continuation。所以 Cont 的类型实际上是: `r -> r`，于是 continuation monad 的类型是 `(r -> r) -> r`。这里 id 输入和结果类型都是一样的，对于其它的 continuation，输入类型和返回结果类型可能不同，那么准确地讲 continuation monad 的类型应该是 `(r1 -> r2) -> r2`。


我们回头看一眼 return 的定义，根据 monad 的要求，return 应该是 `a -> M a`，接受一个值，返回一个 monad。

```
(defun return (x) (lambda (k) (k x)))
```

对！它接受一个值 x，返回一个 monad `(lambda (k) (k x))`，后者就是一个 continuation monad 类型，是一个 `Cont -> r` 的东西， k 就是连续，类型为 Cont, 即 `r1 -> r2`。

然后我们的 fact 函数就可以这么写了，把 if 的第一个分支用 return 简化：

```
(defun fact (acc n)
  (if (= n 0)
      (return acc)
    (lambda (k)
      (fact (* acc n) (- n 1) k))))
```

这个形式已经简化了很多了，通过 curry 化和 return 的变换，我们 “消除” 掉了 CPS 的一部分结构。现在我们的 `(fact acc n)` 是返回一个 continuation monad 的，而不是直接返回结果。

if 的另一个分支可不可以消除掉 CPS 写法？是可以的！ 其实这只是一个 eta 变换而已，两者等价的：

```
(lambda (k)
      (fact (* acc n) (- n 1) k))
==>
(fact (* acc n) (- n 1))
```

试一下：

```
(defun fact (acc n)
  (if (= n 0)
      (return acc)
      (fact (* acc n) (- n 1))))

(fact 1 5 (lambda (x) x))
120
```


(注意，这个是在我设计自己的语言 [cora](https://github.com/tiancaiamao/cora) 里面运行的，不能用普通的 scheme 或者一般的 lisp，因为 cora 支持了自动 curry，一般 lisp 不支持。使用 scheme 会出现参数不匹配的错误)

注意上面最后的那个 fact 的定义，它已经不再是 CPS 形式了！ 但是它又是 CPS 形式的！ 写起来没有那么丑，然而效果上又确实是 CPS 形式，这正是我想要的。


再看一个例子，不用尾递归版本的 fact 函数：

```
(defun fact (n k)
  (if (= n 0)
      (k 1)
    (fact (- n 1)
	  (lambda (res)
	    (k (* n res))))))
```

再是做 curry 以及用 return 改写：

```
(defun fact (n)
  (if (= n 0)
	  (return 1)
	(lambda (k)
	  ((fact (- n 1))
	   (lambda (res)
		   (k (* n res)))))))
```

这一次我们要用上 bind 了，改写 `(* n (fact (- n 1)))` 这个 if 分支。bind 接受一个 monad，以及一个函数。

```
(defun bind (m f)
       ...)
```

bind 的返回值应该是另外一个 monad，所以应该是一个 `(lambda (k) ...)` 这样的东西（如果不记得，看前面的 id 函数，或者是 `Cont -> r` 定义）：

```
(defun bind (m f)
	(lambda (k)
	   .....
```

再然后，要把 monad m 里面的值取出来，需要给它提供一个连续：

```
(m (lambda (res)
        ...)
```

取出来之后，res 是给 f 使用的，f 应该是接受一个值，返回另外一个 continuation：

```
(m (lambda (res)
        (f res)
		    ...
```

我们知道，当 CPS 变换之后，函数永不返回了，而是以参数形式传给 k，所以这里应该是：


```
(defun bind (m f)
	(lambda (k)
		(m (lambda (res)
			((f res) k)))))
```

这就是 bind 的定义了。看看使用。我们要改写 `(* n (fact (- n 1)))` 这个东西。


`(fact (- n 1))` 正是这个 continuation monad，

```
(bind (fact (- n 1))
	...)
```

然后是函数 f，它要从 monad 中获取到 `(fact (- n 1))` 的计算结果 `res`，然后要返回另一个 monad，我们再次使用 return 来制造一个新的 monad。

```
(lambda (res) (return (* n res)))
```

于是用 bind 改写后的 fact 的定义：

```
(defun fact (n)
  (if (= n 0)
      (return 1)
    (bind (fact (- n 1))
	  (lambda (res)
	    (return (* n res))))))
```

运行一下试试：

```
7 #> (fact 5 (lambda (x) x))
120
```

oh, yeah!

好了，再看最后一个例子， 这次用 fib 函数，这个要是用 CPS 格式写，还是很蛋疼的，因为不是尾递归要转换好几次：

```
(defun fib (n)
  (cond
   ((= n 0) (return 0))
   ((= n 1) (return 1))
   (true
    (bind (fib (- n 1))
	  (lambda (x1)
	    (bind (fib (- n 2))
		  (lambda (x2)
		    (return (+ x1 x2)))))))))
```

有点丑，不像 haskell 里面，有 do 的记法，很方便。没关系，lisp 里面有宏嘛，我可以定义一个 monad-do 的宏出来，写起来就方便了：

```
(func rewrite-monad-do-h
      [val m . body] => ['bind m ['lambda [val] (rewrite-monad-do-h body)]]
      [body] => body)

(defmacro monad-do (exp)
  (rewrite-monad-do-h (cdr exp)))
```

```
(defun fib (n)
  (cond
   ((= n 0) (return 0))
   ((= n 1) (return 1))
   (true (monad-do
	  x1 (fib (- n 1))
	  x2 (fib (- n 2))
	  (return (+ x1 x2))))))
```

来试一下：

```
12 #> (fib 10 (lambda (x) x))
55
```

oh, yeah!

说说 continuation monad 的重要意义吧。其实我本来是想在 cora 语言里面加 generator 或者是 yield 之类的实现，没想到一个好的方案。
编译阶段转 CPS 暴露 continuation 可以实现，但生成代码很难读，也不好优化。人肉手写 CPS 完全不靠谱。看了看 delimited continuation 其实也没搞懂怎么样简单地实现。最后不小心看到 continuation monad 上面了，算是意外收获。**通过 continuation monad，我们可以依然写非 CPS 风格的代码，但是确是可以暴露出连续的**，后面就有机会做 `call/cc` 之类的事情了。而且 CPS 非常灵活，控制流自己搞。
