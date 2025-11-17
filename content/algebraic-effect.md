[上一篇](/koka-papers.md)里面提到了，algebraic effect 一定是有某种可以直接转换成基本的 lambda calcalus 的方式的，只是我还不知道如何转。
读论文找答案一方面不确定我应该找哪一篇，另一方面好多论文有些太复杂了，读起来很头大。
今天静下来的时候，独自好好想了想这个转换过程，有点灵感，记录下来。


首先，algebraic effect 可以理解成一种特殊的 try catch，跟 try catch 不同的是，它是可以从 throw 的位置 recover 的。

```
(begin 1 2
	(throw 3)
	4 5 6)
```

由于可以 recover，catch 函数的形式，并不是只接受一个 error 参数，它还会接受一个 continuation 参数，以恢复之前的执行：

```
handler = (lambda (v k)
	...)
```

上面那段 `begin` 的代码，假如我们 `throw 3` 之后，如何恢复 `4 5 6` 的执行呢？ 这个东西必须包装在一个函数里面变成连续才有可能。这个函数其实就是传给 handler 的连续 k，由 handler 去调用它以恢复执行。

```
(begin 1 2
	...
	(lambda (v)
	4 5 6))
```


然后我们换一个角度看 catch 的 handler 过程。它其实可以看成是一个回调。throw 的时候会让这个回调函数做一些处理，处理完之后，还是返回来：

```
(begin 1 2
	(handle 3 (lambda (v)
		4 5 6)))
```

所以我们把中间这一行不看，代码的形状就是这样子：

```
(begin 1 2
	...
	4 5 6)
```

这段代码会抛出一个 3，然后让 handler 把 3 处理一下，返回另一个值，然后代码继续执行下面的 `4 5 6` 的逻辑。
传入 3 传出另一个值的过程，就是副作用处理的过程。这个副作用怎么发生的现在不用管，从整个函数的角度，依然是一个纯的函数。

handler 做的事情，就是把传入的 3 转化成另一个值，再传出去。所以 handler 的定义是这样子：

```
(defun handler (v1 k)
	;; v2 = v1 + 1
	(k v2))
```

最上面的那一段展开之后，就是

```
(begin 1 2
	(handle 3 (lambda (v)
		4 5 6)))
```

这个写法不太好看，我们如果用 `==<` 之类的给它做一下转换，易读一些，比如用 `do` notation 的形式：

```
(begin 1 2
	(do
		v (handle 3)
		4 5 6))
```

大概就相当于

```
(begin 1 2
	v = (handle 3)
	4 5 6)
```

上面这样子是不是就很好读了呢？对比一下完整的形式


```
(try (begin 1 2
			(throw 3)
			4 5 6)
	 (catch (v k)
		 (k (+ v 1))))
```

等价于另一种写法(相当于宏展开吧)：

```
((lambda ( handle)
	(begin 1 2
		(do
			_ (handle 3)
			4 5 6))
 (lambda (v k)
	(k (+ v 1)))))
```

其实叫 `handle 3` 或者叫 `throw 3` 都是一个样子，只是从命名上，区分一个是直接抛出结果，还是从抛出的位置继续往下面执行。


这个过程已经推导出来了。其实就是几个点：

- throw 之后的部分，全部打包成一个连续。
- 这个连续会作为参数 k 传给 effect 的 handler。
- throw 后面的本来是 `lambda (k)...`，用一个 do 的 monad 变换让代码读起来更友好

--------------------------------


假设 handler 里面，不调用 k，而是直接返回，会怎么样？ 效果就等价于普通异常了

```
(defun handler (v k)
	42)
```

相当于直接从 try 的那个 block 里面返回去了。

```
(begin 1 2
	(handle 3 (lambda (v)
		4 5 6)))
```


-----------------------------------


再看一下栈变化过程，跟 runtime 拷贝栈的方式做一下对比。

首先是在 try 的 block 的栈里面， 


```
try ...
...

```

然后调用 handle，假设语言是实现了尾递归的，那么并不是

```
try ...
...
handle ...
...
```

而是直接在 try 的那个栈上恢复 handle：

```
handle ...
...
```


如果 `handle` 不调用 k，也就是等价于抛异常的场景，就直接返回了。而如果继续处理呢，就相当于

```
handle ...
k ...
...
```

也就是继续执行 try 的从 throw 之后的内容，这是正想要的效果。也就是说，这个变换并不需要特殊的 runtime 栈的支持，而达到的效果是一样的。
其中关键的点在于，连续的保存。如果是 runtime 栈，保存那个连接需要把栈拷贝一份出去，供将来恢复。
而在我们的推导变换过程中，这个连续 k 只是一个闭包，而已！

```
(lambda (v)
	4 5 6)
```

这个闭包接受了 handler 处理之后的值，就可以继续往下执行了。

algebraic effect 它的表达能力跟 delimited continuation 是等价的，可以实现异常，generator，async/await，协程等等基础设施。
现在我知道了怎么样直接转成 lambda calcalus，这将会非常有用。


-----------------------------------------------

又发现了一个问题，前面的场景想得太简单了，只是 handle effect 的某一种特殊形式。假设嵌套了多层函数，就不太一样了：

```
try (begin 1 2
	        (f ...)
			4 5 6)

f = (lambda (...)
	    (g ...))

g = (lambda ...
	   (throw 3))
```


f 调用 g，g 调用 h，然后在里面才做 throw，这里如果只能 g 里面 throw 之后部分提取成连续，那效果并不是回到 try 的那一层，在那个位置重新 recover。
它会从 g 做 throw 之后的地方 recover，再一级一级返回 g，返回 f，返回到 try 的调用位置，这显然跟 algebraic effect 的要求是对不上的。


跨函数调用栈之后，连续的处理就变了。需要用 cps 的形式处理：


```
(begin 1 2
       (f ... (lambda (v)
                  4 5 6))

f = (lambda (x .. k)
    	    (g .. k.))

g = (lambda (...k)
    	    (throw 3)
	    ...)

g = (lambda (...  k)
    	    (handle v1 (lambda (v2)
	    	           (k v2))))
```


```
(begin 1 2
       (do
	 v  (f ...)
	 4 5 6))
```

函数 f 必须是 cps 形式，函数 g 必须是 cps 形式，直到 throw 的位置。也就是从 try 开始的点，到中间的多层调用，都需要变成 cps 形式。这就跟 delimited continuation 一个样子了。
如果把前面一篇 [continuation monad](/continuation-monad.md) 跟这一篇结合起来，也不是不能做。

...就是仍然有点麻烦。需要自动地决定是否需要改写成 cps 形式，改写取决于某个函数有没有被 try throw 的调用栈路径上面使用过，如果是，就需要转换。



