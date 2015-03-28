编译器自举是什么意思？以scheme为例，最初的scheme编译器肯定没法用scheme语言写，可能用c或者其它写成。等有了第一个编译器以后，就可以用scheme写了。如果一门语言的编译器用这门语言自身编写，并且能够跑起来，就算能够自举了(Go语言的1.5版正在搞自举呢，题外话)。

无聊**又在**写scheme编译器玩。这次我想用scheme语言写编译器部分，生成字节码输出。再用C语言写一个字节码的虚拟机。等这两部分都弄好了，编译scheme的代码，生成字节码跟虚拟机拼起来，就可以得到一个不依赖外部scheme的编译器了。也就是，可以实现自举。

关于写编译器、解释器，以前也各种折腾过，总是没有做出令自己比较满意的结果。究其原因，写一个很基本的demo并不难，但是处理复杂的语言规范的细枝未节，实现一些高级的语言特征，自己的精力和水平都不行。

这次把目标定低，只玩自举，不求高大上。首先是虚拟机这边，暂时先不要搞垃圾回收，这样可以少死很多脑细胞。JIT什么的了，想都不要去想，性能不是现在考虑的。还有就是虚拟机的体系结构也不必要按标准的来，因为scheme的闭包以及continuation，若是采用c的那套体系结构，要在虚拟机这边做trampoline一类的技术，编译器那边也要对自由变量做lambda lifting，还有CPS变换，会很伤脑细胞。不按标准硬件结构来，设置一个环境寄存器，简单很多。

然后是scheme写编译器，尽量用简单语法写，为了自举而采用一个折中。代码不要用宏语法，因为暂时不打算实现宏。call/cc也是暂时用不到并且暂时不会实现的。有些语法会很好用，但实现略难，那么写代码时就尽量避开这种语法，总之，使实现自举的代价最低。

接下来看看其中一些关键字的语法。

对于define，暂时不支持内部define。因为实现内部define要在编译的第一步做一个重写，类似下面的变换：

	(define a 3)
	=>
	((lambda (a) (set! a 3)) #f)

我太懒了，宁愿不要用到内部define。全局define还是必须要的，否则写代码太痛苦了。

let是要的。let的变种很多，从最基本的let，循环的let语法，以及let*和letrec等。都可以手动做一个类似宏的重写。比如基本的let变换：

	(let ((a 1)
	      (b 3))
	  (+ a b))
	=>
	((lambda (a b)
	    (+ a b))
	  1 3)

至于命名let循环，尽管没有做内部define，但还是可以用Y combinator重写，虽然性能不高：

	(let loop ((n 0)
	           (sum 0))
	  (if (> n 100)
	      sum
	      (loop (+ n 1) (+ sum n))))
	=>
	((Y (lambda (loop)
	     (lambda (n sum)
	       (if (> n 100)
	      sum
	      (loop (+ n 1) (+ sum n))))))
	 0 0)

写到这里的时候发现了一个很蛋疼的问题：call-by-value方式的Y的实现，教材中通常是这样子。

	(define Y
	  (lambda (F)
	    ((lambda (u) (u u))
	     (lambda (x)
	       (F (lambda (v) ((x x) v)))))))

这样写Y是有问题的，它只支持一个参数。虽然可以写成下面这样支持两个参数，但始终不是支持任意参数。

	(define Y
	  (lambda (F)
	    ((lambda (u) (u u))
	     (lambda (x)
	       (F (lambda (v1 v2) ((x x) v1 v2)))))))

尝试利用(lambda x . y)这种变长参数，但是没成功。只好先自己手写几个处理不同参数的Y1,Y2,Y3...

case也暂时不实现，因为没有define-syntax宏不好实现case，暂时代码都用cond写吧。

看一下效果，这段是主结构的代码：

	(define meaning
	  (lambda (e r tail?)
	    (if (atom? e)
	        (if (symbol? e) (meaning-reference e r tail?)
	            (meaning-quotation e r tail?))
	        (let ((syntax (car e)))
	          (cond
	            ((eq? syntax 'quote)  (meaning-quotation (cadr e) r tail?))
	            ((eq? syntax 'lambda) (meaning-abstraction (cadr e) (cddr e) r tail?))
	            ((eq? syntax 'if)     (meaning-alternative (cadr e) (caddr e) (cadddr e) r tail?))
	            ((eq? syntax 'begin)  (meaning-sequence (cdr e) r tail?))
	            ((eq? syntax 'set!)   (meaning-assignment (cadr e) (caddr e) r tail?))
	            ((eq? syntax 'define) (meaning-define (cadr e) (caddr e) r tail?))
	            ((eq? syntax 'let)    (meaning (rewrite-let (cdr e)) r tail?))
	            ((eq? syntax 'let*)   (meaning (rewrite-let* (reverse (cadr e)) (caddr e)) r tail?))
	            ((eq? syntax 'cond)   (meaning (rewrite-cond (cdr e)) r tail?))
	            (else     (meaning-application syntax (cdr e) r tail?)))))))

编译出来字节码是：

	(CREATE-CLOSURE
	2
	GOTO
	673
	ARITY=?
	4
	EXTEND-ENV
	SHALLOW-ARGUMENT-REF
	0
	INVOKE1
	atom?
	JUMP-FALSE
	54
	SHALLOW-ARGUMENT-REF
	0
	INVOKE1
	symbol?
	JUMP-FALSE
	24
	CHECKED-GLOBAL-REF
	1
	PUSH-VALUE
	SHALLOW-ARGUMENT-REF
	0
	PUSH-VALUE
	SHALLOW-ARGUMENT-REF
	1
	PUSH-VALUE
	SHALLOW-ARGUMENT-REF
	2
	PUSH-VALUE
	ALLOCATE-FRAME
	3
	POP-FRAME!
	2
	POP-FRAME!
	1
	POP-FRAME!
	0
	POP-FUNCTION
	FUNCTION-INVOKE
	GOTO
	22
	CHECKED-GLOBAL-REF
	2
	PUSH-VALUE
	SHALLOW-ARGUMENT-REF
	0
	PUSH-VALUE
	SHALLOW-ARGUMENT-REF
	1
	PUSH-VALUE
	SHALLOW-ARGUMENT-REF
	2
	PUSH-VALUE
	ALLOCATE-FRAME
	3
	POP-FRAME!
	2
	POP-FRAME!
	1
	POP-FRAME!
	0
	POP-FUNCTION
	FUNCTION-INVOKE
	GOTO
	609
	SHALLOW-ARGUMENT-REF
	0
	INVOKE1
	car
	PUSH-VALUE
	ALLOCATE-FRAME
	1
	POP-FRAME!
	0
	EXTEND-ENV
	SHALLOW-ARGUMENT-REF
	0
	PUSH-VALUE
	CONSTANT
	quote
	POP-ARG1
	INVOKE2
	eq?
	JUMP-FALSE
	39
	CHECKED-GLOBAL-REF
	2
	PUSH-VALUE
	CHECKED-GLOBAL-REF
	3
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	0
	PUSH-VALUE
	ALLOCATE-FRAME
	1
	POP-FRAME!
	0
	POP-FUNCTION
	PRESERVE-ENV
	FUNCTION-INVOKE
	RESTORE-ENV
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	1
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	2
	PUSH-VALUE
	ALLOCATE-FRAME
	3
	POP-FRAME!
	2
	POP-FRAME!
	1
	POP-FRAME!
	0
	POP-FUNCTION
	FUNCTION-INVOKE
	GOTO
	550
	SHALLOW-ARGUMENT-REF
	0
	PUSH-VALUE
	CONSTANT
	lambda
	POP-ARG1
	INVOKE2
	eq?
	JUMP-FALSE
	57
	CHECKED-GLOBAL-REF
	4
	PUSH-VALUE
	CHECKED-GLOBAL-REF
	3
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	0
	PUSH-VALUE
	ALLOCATE-FRAME
	1
	POP-FRAME!
	0
	POP-FUNCTION
	PRESERVE-ENV
	FUNCTION-INVOKE
	RESTORE-ENV
	PUSH-VALUE
	CHECKED-GLOBAL-REF
	5
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	0
	PUSH-VALUE
	ALLOCATE-FRAME
	1
	POP-FRAME!
	0
	POP-FUNCTION
	PRESERVE-ENV
	FUNCTION-INVOKE
	RESTORE-ENV
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	1
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	2
	PUSH-VALUE
	ALLOCATE-FRAME
	4
	POP-FRAME!
	3
	POP-FRAME!
	2
	POP-FRAME!
	1
	POP-FRAME!
	0
	POP-FUNCTION
	FUNCTION-INVOKE
	GOTO
	483
	SHALLOW-ARGUMENT-REF
	0
	PUSH-VALUE
	CONSTANT
	if
	POP-ARG1
	INVOKE2
	eq?
	JUMP-FALSE
	75
	CHECKED-GLOBAL-REF
	6
	PUSH-VALUE
	CHECKED-GLOBAL-REF
	3
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	0
	PUSH-VALUE
	ALLOCATE-FRAME
	1
	POP-FRAME!
	0
	POP-FUNCTION
	PRESERVE-ENV
	FUNCTION-INVOKE
	RESTORE-ENV
	PUSH-VALUE
	CHECKED-GLOBAL-REF
	7
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	0
	PUSH-VALUE
	ALLOCATE-FRAME
	1
	POP-FRAME!
	0
	POP-FUNCTION
	PRESERVE-ENV
	FUNCTION-INVOKE
	RESTORE-ENV
	PUSH-VALUE
	CHECKED-GLOBAL-REF
	8
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	0
	PUSH-VALUE
	ALLOCATE-FRAME
	1
	POP-FRAME!
	0
	POP-FUNCTION
	PRESERVE-ENV
	FUNCTION-INVOKE
	RESTORE-ENV
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	1
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	2
	PUSH-VALUE
	ALLOCATE-FRAME
	5
	POP-FRAME!
	4
	POP-FRAME!
	3
	POP-FRAME!
	2
	POP-FRAME!
	1
	POP-FRAME!
	0
	POP-FUNCTION
	FUNCTION-INVOKE
	GOTO
	398
	SHALLOW-ARGUMENT-REF
	0
	PUSH-VALUE
	CONSTANT
	begin
	POP-ARG1
	INVOKE2
	eq?
	JUMP-FALSE
	29
	CHECKED-GLOBAL-REF
	9
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	0
	INVOKE1
	cdr
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	1
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	2
	PUSH-VALUE
	ALLOCATE-FRAME
	3
	POP-FRAME!
	2
	POP-FRAME!
	1
	POP-FRAME!
	0
	POP-FUNCTION
	FUNCTION-INVOKE
	GOTO
	359
	SHALLOW-ARGUMENT-REF
	0
	PUSH-VALUE
	CONSTANT
	set!
	POP-ARG1
	INVOKE2
	eq?
	JUMP-FALSE
	57
	CHECKED-GLOBAL-REF
	10
	PUSH-VALUE
	CHECKED-GLOBAL-REF
	3
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	0
	PUSH-VALUE
	ALLOCATE-FRAME
	1
	POP-FRAME!
	0
	POP-FUNCTION
	PRESERVE-ENV
	FUNCTION-INVOKE
	RESTORE-ENV
	PUSH-VALUE
	CHECKED-GLOBAL-REF
	7
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	0
	PUSH-VALUE
	ALLOCATE-FRAME
	1
	POP-FRAME!
	0
	POP-FUNCTION
	PRESERVE-ENV
	FUNCTION-INVOKE
	RESTORE-ENV
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	1
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	2
	PUSH-VALUE
	ALLOCATE-FRAME
	4
	POP-FRAME!
	3
	POP-FRAME!
	2
	POP-FRAME!
	1
	POP-FRAME!
	0
	POP-FUNCTION
	FUNCTION-INVOKE
	GOTO
	292
	SHALLOW-ARGUMENT-REF
	0
	PUSH-VALUE
	CONSTANT
	define
	POP-ARG1
	INVOKE2
	eq?
	JUMP-FALSE
	57
	CHECKED-GLOBAL-REF
	11
	PUSH-VALUE
	CHECKED-GLOBAL-REF
	3
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	0
	PUSH-VALUE
	ALLOCATE-FRAME
	1
	POP-FRAME!
	0
	POP-FUNCTION
	PRESERVE-ENV
	FUNCTION-INVOKE
	RESTORE-ENV
	PUSH-VALUE
	CHECKED-GLOBAL-REF
	7
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	0
	PUSH-VALUE
	ALLOCATE-FRAME
	1
	POP-FRAME!
	0
	POP-FUNCTION
	PRESERVE-ENV
	FUNCTION-INVOKE
	RESTORE-ENV
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	1
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	2
	PUSH-VALUE
	ALLOCATE-FRAME
	4
	POP-FRAME!
	3
	POP-FRAME!
	2
	POP-FRAME!
	1
	POP-FRAME!
	0
	POP-FUNCTION
	FUNCTION-INVOKE
	GOTO
	225
	SHALLOW-ARGUMENT-REF
	0
	PUSH-VALUE
	CONSTANT
	let
	POP-ARG1
	INVOKE2
	eq?
	JUMP-FALSE
	41
	CHECKED-GLOBAL-REF
	0
	PUSH-VALUE
	CHECKED-GLOBAL-REF
	12
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	0
	INVOKE1
	cdr
	PUSH-VALUE
	ALLOCATE-FRAME
	1
	POP-FRAME!
	0
	POP-FUNCTION
	PRESERVE-ENV
	FUNCTION-INVOKE
	RESTORE-ENV
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	1
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	2
	PUSH-VALUE
	ALLOCATE-FRAME
	3
	POP-FRAME!
	2
	POP-FRAME!
	1
	POP-FRAME!
	0
	POP-FUNCTION
	FUNCTION-INVOKE
	GOTO
	174
	SHALLOW-ARGUMENT-REF
	0
	PUSH-VALUE
	CONSTANT
	let*
	POP-ARG1
	INVOKE2
	eq?
	JUMP-FALSE
	81
	CHECKED-GLOBAL-REF
	0
	PUSH-VALUE
	CHECKED-GLOBAL-REF
	13
	PUSH-VALUE
	CHECKED-GLOBAL-REF
	14
	PUSH-VALUE
	CHECKED-GLOBAL-REF
	3
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	0
	PUSH-VALUE
	ALLOCATE-FRAME
	1
	POP-FRAME!
	0
	POP-FUNCTION
	PRESERVE-ENV
	FUNCTION-INVOKE
	RESTORE-ENV
	PUSH-VALUE
	ALLOCATE-FRAME
	1
	POP-FRAME!
	0
	POP-FUNCTION
	PRESERVE-ENV
	FUNCTION-INVOKE
	RESTORE-ENV
	PUSH-VALUE
	CHECKED-GLOBAL-REF
	7
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	0
	PUSH-VALUE
	ALLOCATE-FRAME
	1
	POP-FRAME!
	0
	POP-FUNCTION
	PRESERVE-ENV
	FUNCTION-INVOKE
	RESTORE-ENV
	PUSH-VALUE
	ALLOCATE-FRAME
	2
	POP-FRAME!
	1
	POP-FRAME!
	0
	POP-FUNCTION
	PRESERVE-ENV
	FUNCTION-INVOKE
	RESTORE-ENV
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	1
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	2
	PUSH-VALUE
	ALLOCATE-FRAME
	3
	POP-FRAME!
	2
	POP-FRAME!
	1
	POP-FRAME!
	0
	POP-FUNCTION
	FUNCTION-INVOKE
	GOTO
	83
	SHALLOW-ARGUMENT-REF
	0
	PUSH-VALUE
	CONSTANT
	cond
	POP-ARG1
	INVOKE2
	eq?
	JUMP-FALSE
	41
	CHECKED-GLOBAL-REF
	0
	PUSH-VALUE
	CHECKED-GLOBAL-REF
	15
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	0
	INVOKE1
	cdr
	PUSH-VALUE
	ALLOCATE-FRAME
	1
	POP-FRAME!
	0
	POP-FUNCTION
	PRESERVE-ENV
	FUNCTION-INVOKE
	RESTORE-ENV
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	1
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	2
	PUSH-VALUE
	ALLOCATE-FRAME
	3
	POP-FRAME!
	2
	POP-FRAME!
	1
	POP-FRAME!
	0
	POP-FUNCTION
	FUNCTION-INVOKE
	GOTO
	32
	CHECKED-GLOBAL-REF
	16
	PUSH-VALUE
	SHALLOW-ARGUMENT-REF
	0
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	0
	INVOKE1
	cdr
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	1
	PUSH-VALUE
	DEEP-ARGUMENT-REF
	1
	2
	PUSH-VALUE
	ALLOCATE-FRAME
	4
	POP-FRAME!
	3
	POP-FRAME!
	2
	POP-FRAME!
	1
	POP-FRAME!
	0
	POP-FUNCTION
	FUNCTION-INVOKE
	RETURN
	SET-GLOBAL!
	0)

----------------

写到这里突然发现这篇文章毫无意义，看得懂的人很容易看得懂我说什么(都是废话)，看不懂的人始终无法理解我在讲什么。

好吧，如果读者看不懂，又想要看懂，这些资料可能有帮助：

* 《Lisp in small pieces》这本书不错，主要参考了里面很多东西，看个大概就知道怎么写scheme编译器了
* [urscheme](http://canonical.org/~kragen/sw/urscheme/), 这个例子很好，2000多行代码就可以写一个scheme到x86汇编的编译器哦
* [nanopass](https://github.com/nobutaka/nanopass) 可以拆成很细的许多步骤，每步只完成一点点事情，最终实现一个编译器。

------------2015.3.28 更新----------

支持变长参数的Y可以这么写:

	(define Y
	  (lambda (f)
	    ((lambda (u) (u u))
	     (lambda (x) (f (lambda v (apply (x x) v)))))))

`lambda v ...`这种形式的lambda表达式中v可以绑定整个参数列表。