## overview

如何将scheme代码编译成c代码？其实只要弄清楚scheme跟c的区别，然后解决几个问题就行了。

scheme语言中是有闭包的，而c则不支持闭包。需要用closure-convert解决这个问题。

scheme语言中有call/cc，最简单的方法就是做cps变换。

cps变换之后，程序不会返回到调用者，放在c语言，栈会一直增长。如何处理这个问题？Cheney on the M.T.A.

## cps-convert

做cps变换的主要目的是支持call/cc。假设输入是

	(define fact 
		(lambda (n)
			(if (= n 0)
				1
				(* n (fact (- n 1))))))

经过cps变换，它成为

	(define fact 
	  (lambda (n k5980)
	    (if (= n 0)
		(k5980 1)
		(fact (- n 1)
		      (lambda (rv$5981)
			(k5980 (* n rv$5981)))))))

主要参考[这篇文章](http://matt.might.net/articles/cps-conversion/)。浅显易懂，混合变换很实用。

王垠的40行代码只是处理了最基本的lambda演算，而且并不好懂。跟着这篇[文章](http://tieba.baidu.com/p/2714253120)的解释，才勉强能读懂这40行代码做了什么。

cps变换中需要注意的一个地方是begin的处理。如果按照参考的那篇文章做，

	(T-c (begin 1 2 3) 'cont)

变换完之后会变成

	(cont 3)

中间的1和2被丢弃了。在有副作用的情况下，这样子是不对的。比如

	（begin (set! a 1) 2)

里面的set!就不能丢弃。

## closure-convert

这位国际友人写过另一篇[闭包变换的文章](http://matt.might.net/articles/closure-conversion/)，不过不推荐这个实现。只需要理解其中flat closures和shared closures的概念就够了。其实作者的[这篇文章](http://matt.might.net/articles/compiling-scheme-to-c/)中，有一个closure-convert的源代码，易读性高一些。

closure变换比cps变换要容易得多。知道它要干什么，不参考资料自己应该也能撸出来的。闭包跟c函数的区别，闭包是可以拥有自己的自由变量的
。也就是函数+环境=闭包。为了编译成c，将函数和环境拆分出来。这样函数直接对应到c的函数，环境可以用c结构体表示，而闭包则是一个包含函
数指针和环境的结构体。

用c代码表示很容易理解

	struct Closure {
	  Lambda lam ;
	  Value env ;
	};

使用的时候这样调用：

	struct Closure *c;
	c->lam(c->env, ...);

为了方便中间代码的表示，这里定义了一个closure表达式，其中一部分是一个lambda表达式，另一部分是这个闭包的环境。lambda部分接受的多了一个env参数，将env传给这个参数。

上一步cps变换后的代码，再经过closure-convert，会变成下面这个样子：

	(closure
	 (lambda (env6146 n k5980)
	   (if (= n 0)
	       (k5980 1)
	       ((env-get 0 env6146) (- n 1)
		(closure
		 (lambda (env6145 rv$5981)
		   ((env-get 0 env6145) (* (env-get 1 env6145) rv$5981)))
		 (env-make 2 k5980 n)))))
	 (env-make 1 fact))

## explicit-allocation

转换为c之后，变量是在栈上分配的。explicit-allocation将空间分配的操作也提取出来。转换后的语法中，加入了一个locate表达式。

	(locate ((变量名1 类型 大小)
			 (变量名1 类型 大小))
		...)

在转换后的中间代码语法中，我加入了closure，locate。这种自定义语法的方法在nanopass中早已领教过了。换作其它语言就没法这么玩，lisp语言的天然优势呢！

比如说:

	(env-make 1 fact)

经过explicit-allocation变换之后，将成为:

	(locate ((tmp8528 ENV 2))
		(InitEnv tmp8529 1 fact))

这样做有什么好处呢？采用这个中间代码表示会使得生成c代码的时候特别方便。上面这段代码直接可以转换成c代码的这种写法：

	struct Env tmp8528; // 栈上分配空间
	InitEnv(&tmp8528, 1, fact); // 初始化

之前的fact的代码经过explicit-allocation处理之后，如下所示：

	(locate ((tmp8526 CLOSURE 2)
		 	 (tmp8529 ENV 1))
		(InitClosure tmp8526
			     (lambda (env6146 n k5980)
			       (locate ((tmp8527 CLOSURE 2)
							(tmp8528 ENV 2))
				       (if (= n 0)
					   (k5980 1)
					   ((env-get 0 env6146) (- n 1)
					    (InitClosure tmp8527
							 (lambda (env6145 rv$5981)
							   ((env-get 0 env6145) (* (env-get 1 env6145) rv$5981)))
							 (InitEnv tmp8528 2 k5980 n))))))
			     (InitEnv tmp8529 1 fact)))

## Cheney on the M.T.A

经过cps变换过的代码，是不会返回的。转换成c以后，栈会不停地增长。为了处理这个问题，要用到Cheney on the M.T.A这个算法。

基本原理就是使用垃圾回收将被使用到的数据收集起来。然后利用setjmp/longjmp跳转来回收栈空间。具体的可以用这个关键字自己去搜索。

## GC

[这篇文章](http://www.more-magic.net/posts/internals-gc.html)中描述了GC的过程。其实算法特别简单，用的是一个扫描复制的垃圾回收算法。

但是我实现的时候倒是写得好痛苦。主要是调试很花精力。就单说一个小细节，下面两种表示，哪一种更好呢？

	struct Vector {
		int size;
		Value v[];
	};

	struct Vector {
		int size;
		Value *v;
	};

前一种是在结构体中分配数据的空间。后一种是用的指针，指向的数组空间在别处分配。写过c代码的可能喜欢用前一种。但是这里用前一种做法是有问题的。为什么呢？因为vector可能会resize的，而前一种写法在resize之后对象地址会变。这是不可接受的，比如某个cons中可能有引用着这个Vector呢！

采用了后一种表示。在GC的时候，拷贝Vector对象单拷贝结构体是不够的，需要把指针的数组也拷贝过去。我在这个地方就被坑了，调试了好久才发现问题。

在栈上分配动态大小的空间可以用alloca函数做。反正经过cps变换之后函数是不会直接返回的，所以放心的使用栈空间。

## desugar

desugar应该是在最早做的。要做的事情就是处理语法糖。

在写GC的时候，才意识到MinorGC中不扫描全局对象，那么在栈上为全局变量分配空间是不行的。看下这个代码片断：

	Value fact; // 全局变量
	
	void __lambda_tmp12() {
		struct Closure tmp1321;
		fact = InitClosure(&tmp1312, ...);
	}

fact的空间在栈上分配，在GC的时候这块空间被移到堆上去了。但是fact变量并不会被修改到指向新的空间。

为了处理这种情况，我想要在desugar中去掉全局变量。

	(define fact ...)
	(fact 5)

可以改写为

	((lambda ()
		(define fact ...)
		(fact 5)))

再接着做一个desugar过程，内部define提升到参数中，可以变换成

	((lambda (fact)
		(set! fact ...)
		(fact 5)))

但是这样做有什么问题呢？修改之后的函数的参数个数不知道该如何填充了。比如原来

	(define f (lambda (n) ...))
	(f 3)

如果变换后，该填充几个参数？是调用(f 3)呢？还是(f 3 '())？或者(f 3 '() '() '())

还有代码生成那边一些东西，就不说了。[这里](https://github.com/tiancaiamao/yasfs/tree/master/scheme2c)是我写的一些零零碎碎的代码片断。

只做了这些吧。验证一个东西和实现其中一些技术要点是很好玩的，但是要做完善和处理许多小细节就非常坑了。