最近一次[提交](https://github.com/tiancaiamao/cora/pull/12/files#diff-bd792def7de6d4b8d70ddc844eabe52f73c264904e263580215219b669a2ca9cR417-R495)，在 cora 里面把 resumable exception 给实现了。

resumable exception 和 delimited continuation 或 algebraic effect，其实都是差不多的概念，就是用法和接口看起来不一样而已。在 common lisp 里面的 condition 系统， 就是 resumable exception，而到了后面 paper 里面的概念，就变成了 algebraic effect，新瓶装旧酒。

先看看普通的异常，可以用这样的形式表示：

```
(try (lambda () (throw 42))
	(lambda (e) ...))
```

try 在执行前一段代码块时，如果遇到了 throw，则跳转到后一段的 handler 函数中处理，也是后面可以 catch 住前面 throw 出来的异常。

而 resumable exception，和普通异常的区别是，它是**可以恢复到之后 throw 的位置继续执行**的：


```
(try (lambda () (+ (throw 42) 1))
	(lambda (e cont)
		(cont 666)))
```

这段代码会执行到 throw，然后逻辑跳转到异常处理中，注意 handler 接受两个参数，第一个是 throw 出来的异常 `e`，第二个参数，则是抛出异常位置的 continuation。
当调用 `(cont 666)` 之后，代码会返回到 throw 位置继续执行，也就是 `(+ 666 1)` 得到最终的执行结果 667。

resumable exception 和普通异常的区别就是在于普通异常之后，原始的栈就丢失了，异常处理里面只能做少量清理工作。而 resumable exception 则强大得多，它实际上就是一个 delimited continuation，边界是从 try 到 throw 区间的栈。由于是有界的，比 `call/cc` 的性能会好一些。

我之前用纯库的形式实现了 [algebraic effect](algebraic-effect2.md)，但是那样子实现有一些局限性。举个例子，这段代码用以前那个库就跑不过:

```
(defun f (n)
  (if (= n 0)
      (yield v (eff 'C n)
	     v)
    (+ (f (- n 1)) 1)))

(with-handler ['C (lambda (v k) (k v))]
	      (f 300))
```

或者这样一个例子也跑不过:

```
(with-handler ['C (lambda (v k) (k v))]
	      (map (lambda (x)
		     (yield v (eff 'C x)
			    v))
		   [1 2 3 4 5]))
```

这些局限性的本质，可能一方面是来自于 yield 弱鸡了，它并不是真正的 generator，而是把剩下的计算(continuation)包装成 `(lambda (v) ...)`，返回了 value + continuation 的 tuple。
所有非 tail 位置的调用都会有问题。`(+ (f (- n 1)) 1)` 实际变成了 `(+ [tuple result continuation] 1)` 这类型显然是匹配不上了的。


另一方面，可能这个问题的本质还是在 monad 的不可组合性。之前有一篇[文章](continuation-monad.md)里面，我是写了非尾递归的时候，要用 monad-do 的写法，所以猜想这里有可能要写成这种形式:

```
(monad-do
	x (f (- n 1))
	(return (+ x 1)))
```

delimited continuation 本身确实是可以用[纯库实现](https://github.com/tiancaiamao/cora/blob/9f05ad9f502aaea331d3fa96c371d96d4766bdbc/lib/delimcc.cora)的，不过绕不过 monad。让程序员去尝试理解 "自函子范畴上的一个幺半群" 很容易造成永久性脑损伤。monad 方式实现后，上面 map 的例子就是不可组合性的体现。

这次实现 resumable exception，我是在语言层面实现的，而不是库的形式。换了实现方式，也因此克服了前面的问题。新的实现中，这种代码都是可以跑的:

```
(try (lambda () (map (lambda (x) (throw x)) [1 2 3 4 5]))
     (lambda (v cont) (cont v)))

(defun f (n)
  (if (= n 0)
      (throw 42)
    (+ (f (- n 1)) 1)))

(try (lambda ()
       (f 300))
     (lambda (v cc) (cc v)))
```

其实就是按 [gambit 的做法](gambit-callcc.md) 实现的。关键词：segment stack。

![image](https://user-images.githubusercontent.com/1420062/208905899-c26c73ee-5c3a-43e4-ad67-caec44c8e327.png)

step1 是刚调用到 try 的时候栈的样子，chunk 和 handler 分别是参数。从左往右看。

![image](https://user-images.githubusercontent.com/1420062/208906410-77cf27e4-d15b-45ff-9da6-4cfd3a43666e.png)

step2 是开始执行 chunk 的内容。注意，这里是新分配了一个 segment 栈，在新的栈里面执行 chunk，而不是在原栈上面。用的 segment stack。

![image](https://user-images.githubusercontent.com/1420062/208906446-d907ebe2-2992-4657-b155-abaa4380f262.png)

step3 在 chunk 函数继续执行，它又调用了 throw，进入到 throw 的栈里面处理。throw 会做这样两件事情：一个是把当前位置，一直到 segment stack 的起点的所有栈桢，打包起来，伪装成一个 closure 对象。另一个是，回退到 segment stack 之前的栈，在那里开始调用 handler 函数。

![image](https://user-images.githubusercontent.com/1420062/208906479-c49be739-a091-4f2a-87af-ab60119f1dfb.png)

step4 进入到了 handler 的栈，前面的 throw 在调 handler 的时候已经为它把参数准备好了，第一个参数是 throw 的 v，第二个参数是 continuation，也就是用之前 segment stack 打包伪装而成的一个 closure 对象。

![image](https://user-images.githubusercontent.com/1420062/208906513-7213f54f-93df-467d-9131-75931d4d6195.png)

step5 是如果 resume 之后，会把打包的 segment stack 恢复出来，于是之前 throw 位置的 continuation 也就恢复了。继续往后执行就是正常的函数调用协议了。

关于 segment stack，这里还有一个小技巧。可以利用操作系统的分页机制。开始每个 segment stack 分配比较大的内存空间，实际上只是虚拟地址空间。只有地址真正被访问到的时候，才会发生分页中断，操作系统执行实现的虚拟地址到物理页的映射。

说起来概念好像挺简单，调试代码还是有一丢丢痛苦的。
