在 scheme 语言里面，有个 let loop 宏：

```
(let Label ((Var Init) ...) Body)
```

比如说从 1 加到 100

```
(let loop ((n 1) (sum 0))
	(if (= n 100)
		sum
		(loop (+ n 1) (+ sum n))))
```

在 cora 语言里面，我想引入一个通用的宏用于写循环，不过 `let` 这个 syntax 已经被占用了，所以就叫 `let-loop` 吧。我想这样写：

```
(let-loop recur (n 0 sum 0)
	  (if (= n 100)
	      sum
	    (recur (+ n 1) (+ sum n))))
```

如果展开成这样子，会引入 `recur` 的定义，污染全局命名空间，我不想有这种副作用：


```
((set 'recur (lambda (n sum)
	       (if (= n 100)
		   sum
		 (recur (+ n 1) (+ sum n)))))

 0 100)
```

第一版，错误的实现

```
((let recur (gensym 'tmp)
     (set recur (lambda (n sum)
		  (if (= n 100)
		      sum
		    (recur (+ n 1) (+ sum n))))))
 0 100)
```

如果展开成这样子，lambda 函数里面的 recur 其实是一个 symbol 而不是一个 function，所以会出错。
 
按照 scheme 的搞法，应该用 `letrec`，或者是 `set!` 去改变量的值。

```
(lambda ()
	(define recur (void))
	(define fn (lambda (n sum)
	             (if (= n 100)
					 sum
					 (recur (+ n 1) (+ sum n)))))
	(set! recur fn)) 
```

cora 的 `set` 其实跟 scheme 的 `set!` 是不一样的语义。在 cora 中 `set` 是设计成一个函数的，它就是给一个 symbol 绑定一个值。哪怕这样写：

```
(lambda (f)
	(set f xxx))
```

在 cora 中整个语义是：f 的值，必须是一个符号，然后调用函数把这个符号的值设置为 xxx。在 scheme 中 `set!` 是可以重新绑定变量的。cora 更加"纯"函数一些，没有这种副作用，变量不能改。
 
第二版，正确，但是丑。
	          
```
(let recur (gensym 'tmp)
     (set recur (lambda (n sum)
		  (if (= n 100)
		      sum
		    ((value recur) (+ n 1) (+ sum n))))))
```

想了一个方法是，弄个 value 函数从 `recur` 这个符号得到对应的值，这样就可以了。然而写代码总是有点不太爽，因为不能直接 `recur` 而是 `(value recur)` 感觉怪怪的。

由于 cora 不能用 set 把方式搞，只能想其它办法。比如，Y 组合子：


```
(Y (lambda (recur)
	(lambda (n sum)
		(if (= n 100)
			sum
			(recur (+ n 1) (+ sum n))))))
```

但是 Y 组合子有一个问题，是 cora 不是 lazy evaulation 的，因此要走一个 eta 变换 (alpha, beta, eta ...)，否则会死循环。

`(f f)` => `(lambda (x) ((f f) x))`

走 eta 变换这里又会遇到变参函数问题。有可能需要不一样的参数数量：

`(f f)` => `(lambda (x y) ((f f) x y))`

在 scheme 里面处理方式是用的 apply，这个是可以支持可变参数的：

`(f f)` => `(lambda a (apply (f f) a))`

回到 Y 组合子的推导过程，我发现 let loop 宏在 cora 里面还是可以实现的。

```
(lambda (recur)
	(lambda (n sum)
		(if (= n 100)
			sum
			((recur recur) (+ n 1) (+ sum n)))))
```

注意不再是 recur，而是 `(recur recur)`，因为 recur 是接受一个函数才返回 `(lambda (n sum) ...)`，接受的函数是它自己。

再叫 `recur` 不太准确，做个 alpha 变换，改叫 `meta`

```
(lambda (meta)
	(lambda (n sum)
		(if (= n 100)
			sum
			((meta meta) (+ n 1) (+ sum n)))))
```

cora 是自动做 curry 的，函数定义可以写到一起 `(lambda (meta n sum) ...)`, 再把 `(meta meta)` 提取出来，叫 `recur`

最终版本

```
(let f (lambda (meta n sum)
	  (let recur (meta meta)
	       (if (= n 100)
		   sum
		 (recur (+ n 1) (+ sum n)))))
     (f f))
```
