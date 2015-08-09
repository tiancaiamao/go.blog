以前看scheme时，记得scheme是说只有几个基本的core syntax。而primitive并没有算做这门语言的必须部分。当时觉得不可思议，这门语言怎么能离得开cons,car,cdr这些基本的primitive。不过当时也没有深究。

再回头看SICP时，有点震惊了。原来之后以说这些primitive并不算是语言必须的部分，是因为即使是一些primitive，用lambda,if,begin,define,quote等一些基本的元素也是可以定义出来的。scheme直接对计算本质的抽象是不需要机器的。

先看看如何用最基本的元素定义出cons,car,cdr:

	(define (cons x y)
	  (lambda (n)
	    (if (= n 0)
		x
		y)))
	(define (car z)
	  (z 0))
	(define (cdr z)
	  (z 1))

或者这样子：

	(define (cons x y)
	  (lambda (m)
	    (m x y)))
	(define (car z)
	  (z (lambda (p q) p)))
	(define (cdr z)
	  (z (lambda (p q) q)))

太强大了，只使用基本的core syntax就把cons这种primitive定义出来的。这个其实是做了一个闭包，将cons的参数值保存在了闭包中，然后car和cdr将保存的值从闭包中取出来。其实还有好多种办法定义出cons，比如下面利用的是从数字2^a * 3^b中可以解析得到a和b。

	(define (cons a b)
	  (* (expt 2 a) (expt 3 b)))
	(define (car z)
	  (if (= 0 (remainder z 2))
	      (+ 1 (car (/ z 2)))
	      0))
	(define (cdr z)
	  (if (= 0 (remainder z 3))
	      (+ 1 (cdr (/ z 3)))
	      0))

类似地，像数字0，加法，也可以用基本的core syntax定义出来。比如，我们假设0就是0个#t的list，1是1个#t的list，n是n个#t的list，则：

	(define zero '())
	(define (zero? v) (null? v))
	(define (add1 v) (cons #t v))
	(define (sub1 v) (cdr v))
	(define (+ x y)
	  (cond
	   ((zero? x) y)
	   ((zero? y) x)
	   (else
	    (+ (add1 x) (sub1 y)))))

有了加法之后，定义乘法也就不是什么难事。

	(define (* a b)
	  (if (= b 1)
	      a
	      (+ a (* a (- b 1)))))

太神奇了，lambda真的是对计算本质的抽象，完全不信赖机器。

再回头来想，if为什么必须定义为core syntax而不是能procedure/primitive。因为求值顺序，用produce定义if会产生副作用。如果是用primitive定义的if，那么 (if test true false)中不管test是真还是假，true和false子句都是被执行的。
