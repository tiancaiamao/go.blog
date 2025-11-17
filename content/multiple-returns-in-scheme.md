像C那样的语言可以通过传返回值的指针的方式，实现多值返回。一直没想明白像scheme这种语言怎样实现多值返回，后来在the little schemer中找到方法了。

假设有个函数，它接受的参数是整数的list，返回所有list中的奇数构成的list，并返回所有的偶数之和，偶数之积。这就是一个需要返回多个值的场景。

```scheme
(define func
  (lambda (lon col)
    (if (null? lon)
        (col '() 0 1)
        (if (odd? (car lon))
            (func (cdr lon)
              (lambda (l sum fac)
                (col (cons (car lon) l) sum fac)))
            (func (cdr lon)
              (lambda (l sum fac)
                (col l (+ (car lon) sum) (* (car lon) fac))))))))

(func '(1 2 3 4 5 6 7) (lambda (l sum fac) (cons sum (cons fac l))))
```

scheme中地道的多值返回是通过collector实现的。

-------------------------

2015.6.9更新

上面的做法其实本质上就是自己手动做CPS转换。类似的做法，用Monad也可以实现。

标准的scheme有values函数可以实现多值返回，相关的函数还有define-values，let-values，call-with-values等。

------------------

2023.11.5更新

如果用 monad 写法，推导过程类似于 [continuation monad 推导](/continuation-monad.md)，做 curry 把 cps 写法中的 return 隐藏起来。
用 cora 表示如下，最初的 cps 写法:

```
(func f
      [] return => (return () 0 1)
      [a . b] return => (if (odd? a)
			    (f b (lambda (l sum fac)
				   (return [a . l] sum fact)))
			  (f b (lambda (l sum fac)
				 (return l (+ (car lon) sum) (* (car lon) fact))))))
```

经过 curry 之后:

```
(func f
      [] => (lambda (k) (k () 0 1))
      [a . b] => (if (odd? a)
		     ((f b)
		      (lambda (l sum fac)
			(lambda (k)
			  (k [a . l] sum fact)))
		      ((f b)
		       (lambda (l sum fac)
			 (lambda (k)
			   (k l (+ (car lon) sum) (* (car lon) fact))))))))
```


再变成 monad 写法:

```
(defun return (a b c)
  (lambda (k)
    (k a b c)))

(defun bind (m f)
  (m f))

(func f
      [] => (return () 0 1)
      [a . b] => (if (> a 3)
		     (bind (f b)
			   (lambda (l sum fac)
			     (return [a . l] sum fac)))
		   (bind (f b)
			 (lambda (l sum fac)
			   (return l (+ a sum) (* a fac))))))
```

monad 写法中的 return a b c 其实是一个 eta 变换： x == (lambda (k) (k x))
