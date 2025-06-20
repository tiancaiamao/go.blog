函数式语言里面不带副作用，实现状态修改需要一些技巧性的写法，也就是 state monad。

下用面一个例子来推导 state monad。先来一个假设的需求，我们要将一个 sexp 里面出现的所有数字 +1，同时，还需统计这个 sexp 里面出现的符号 a 的次数。

在 cora 语言里面是没有局部变量修改操作，它是一门函数式的语言，虽然不像 haskell 那么纯函数式。
它的符号是可以重绑定的，以及 vector 是可以修改的。所以有绕过去的写法，通过符号的重赋值：

```
(set '*var* (gensym))

(func parse
      'a => (begin
		 (set *var* (+ (value *var*) 1))
		 'a)
      x => (+ x 1) where (number? x)
      [x . xs] => [(parse x) . (parse xs)]
      x => x)

(set *var* 0)
(parse `(a 3 (4 2 5 (a) b (d (c)) 7 2)))
(value *var*)
```

这里的 *var* 是一个符号，通过 (value *var*) 和 (set *var* xxx) 来实现类似全局变量的效果，通过它统计 a 的次数。
这种写法的代码很糟糕，它是带副作用的。


怎么样写不带副作用的代码呢？我们需要给 parse 加一个参数，它既是一个传入参数，也是一个返回值参数：


```
(func parse
      'a s => [(+ s 1) 'a]
      x s => [s (+ x 1)] where (number? x)
      [x . xs] s => (match (parse x s)
			   [s1 x1]
			   (match (parse xs s1)
				  [s2 xs1]
				  [s2 [x1 . xs1]]))
      x s => [s x])
```

使用的时候每次都需要传入参数 s，这样就没有副作用了

```
(let s 0
     (parse `(a 3 (4 2 5 (a) b (d (c)) 7 2)) s))
```

s 其实是一个状态，调用 parse 函数其实是需要修改这个状态。parse 是一个"有副作用" 的方法，它需要返回 parse 后的结果，并且同时需要修改状态 s。实现方式是传入旧的状态，返回新的状态这种形式的 "修改"，所以它又是无副作用的。

对于每个有状态的函数，我们都要额外传状态参数，这其实会让人很恼火。怎么样把这个状态参数消除掉呢？

我们可以先进行 curry 变换，把 s 从参数挪出去：

```
(func parse
      'a => (lambda (s) [(+ s 1) 'a])
      x => (lambda (s) [s (+ x 1)]) where (number? x)
      [x . xs] => (lambda (s)
		    (match (parse x s)
			   [s1 x1]
			   (match (parse xs s1)
				  [s2 xs1]
				  [s2 [x1 . xs1]])))
      x => (lambda (s) [s x]))

((parse  '(a 3 (4 2 5 (a) b (d (c)) 7 2))) 0)
```

接下来，我们重点为看 (parse x s) 的返回处理，

```
(match (parse x s)
       [s1 x1]
	   ...body)
```

这是一个多值返回，在[多值返回那篇博客](multiple-returns-in-scheme.md)中，我们记得*返回多个值 s1 x1 等价于接受一个 k 然后调用 (k s1 x1)*。


```
(match (parse x s)
	[s1 x1]
	(k s1 x1))

k = (lambda (x1)
      (lambda (s1)
	... ;; 接受多值后的处理，body))
```

于是前面的 parse 函数我们就可以改写成：

```
(func parse
      'a => (lambda (s) [(+ s 1) 'a])
      x => (lambda (s) [s (+ x 1)]) where (number? x)
      [x . xs] => (lambda (s)
		    (match (parse x s)
			   [s1 x1]
			   (k x1 s1)))
      x => (lambda (s) [s x]))


k = (lambda (x1)
      (lambda (s1)
	(match (parse xs s1)
	       [s2 xs1]
	       [s2 [x1 . xs1]])))
```

我们当然可以很自由地修改 (parse x s) 这种变成 ((parse x) s)，(k x1 s1) 也可以 ((k x1) s1)，或者如果我们不喜欢 k 这个名字，我们也可以改名叫 f，都是 curry 变换和 alpha 变换。于是 parse 可以变成这样子:


```
(func parse
      'a => (lambda (s) [(+ s 1) 'a])
      x => (lambda (s) [s (+ x 1)]) where (number? x)
      [x . xs] => (lambda (s)
		    (match ((parse x) s)
			   [s1 x1]
			   ((f x1) s1)))
      x => (lambda (s) [s x]))


f = (lambda (x1)
      (lambda (s1)
	(match (parse xs s1)
	       [s2 xs1]
	       [s2 [x1 . xs1]])))
```

我们把其中 [x . xs] 处理分支拧出来看:

```
(lambda (s)
		    (match ((parse x) s)
			   [s1 x1]
			   ((f x1) s1)))
```

把 (parse x) 改成参数 m

```
(lambda (s)
		    (match (m s)
			   [s1 x1]
			   ((f x1) s1)))

m = (parse x)
```

于是我们重新发明了 state monad 的 bind 函数:

```
(defun bind (m f)
  (lambda (s)
    (match (m s)
	   [s1 v]
	   ((f v) s1))))
```

我们可以用 bind 改写 parse 函数：

```
(func parse
      'a => (lambda (s) [(+ s 1) 'a])
      x => (lambda (s) [s (+ x 1)]) where (number? x)
      [x . xs] => (bind (parse x) f)
      x => (lambda (s) [s x]))


f = (lambda (x1)
      (lambda (s1)
	(match (parse xs s1)
	       [s2 xs1]
	       [s2 [x1 . xs1]])))
```

其实 f 也是可以用 bind 继续改写的，注意 f 的 body 部分，变换技巧也是跟前面一样，把 match 之后的多值返回，返回多个值等价于传递一个连续参数 k1:

```
(lambda (s1)
	(match ((parse xs) s1)
	       [s2 xs1]
		   (k1 xs1 s2)))

k1 = (lambda (xs1)
	  (lambda (s2)
	       [s2 [x1 . xs1]]))
```

于是变成了 (bind f k1)

也就是说前面的 parse 可以最终写成这个样子：

```
(func parse
      'a => (lambda (s) [(+ s 1) 'a])
      x => (lambda (s) [s (+ x 1)]) where (number? x)
      [x . xs] => (bind (parse x)
			(lambda (x1)
			  (bind (parse xs)
				(lambda (xs1)
				  (lambda (s2)
				    [s2 [x1 . xs1]])))))
      x => (lambda (s) [s x]))
```

我们定义 state monad 的 return：

```
(defun return (v)
  (lambda (s)
    [s v]))
```

于是 parse 可以继续用 return 改写：

```
(func parse
      'a => (lambda (s) [(+ s 1) 'a])
      x => (return (+ x 1) where (number? x)
      [x . xs] => (bind (parse x)
			(lambda (x1)
			  (bind (parse xs)
				(lambda (xs1)
				  (return [x1 . xs1]))))))
      x => (return x))
```

大功告成！这已经是改成用 monad 写法了。再看一看 state monad 是什么：


```
(defun bind (m f)
  (lambda (s)
    (match (m s)
	   [s1 v]
	   ((f v) s1))))

(defun return (v)
  (lambda (s)
    [s v]))
```

bind 会接受一个 state monad，以及处理从 monad 提取到的值的函数 f，它会返回一个新的 state monad。
而 state monad 是什么呢？是一个接受一个状态，返回新的状态和新的值的 closure。从 return 的定义来看这一点理解得更清晰。

如果用闭包和对象等价的角度来看("对象是穷人的闭包")，state monad 是一个对象，"这个对象里面有一个 field 存了状态，当在这个对象上面执行操作的时候，不是直接修改这个对象的 field，而是返回一个新的对象，新的对象中的状态被更新了"。是这样的么？不是！状态是需要传进去的，而不是存储在对象的 field 里面的。需要给这个对象传一个 state，就可以"激活"这个对象了。其实值才是对象中的 field，而状态不是。

(m s) 让 monad 接受一个状态，就会返回状态和值。用 match (m s) 去处理返回的状态和值，值传递给 f 去生成新的 monad，新的 monad (f v) 再接受老的状态 s1 更新到新的状态和新的值。好绕！



----------

2025-06-12 更新

发现一个更简单的推导

最初的 (set '*var* (gensym)) 的方式是不纯的，因此需要改写 parse 成一个纯函数。

parse 首先需要补一个参数 s，这个输入的状态，也是将要返回的状态。
由于要同时返回新状态，以及 parse 完的结果，这是一个多值返回，用 cps 的技巧来搞多值返回，需要再被一个参数 k

于是 parse 的写法：


```
(func parse
      'a s k => (k (+ s 1) 'a)
      x s k => (k s (+ x 1)) where (number? x)
	  [x . xs] s k => (parse x s
				 (lambda (s1 x1)
				   (parse xs s1
					  (lambda (s2 xs1)
					(k s2 [x1 . xs1]])))))
      x s k => (k s x))
```

把 s 和 k 两个参数都 curry 化:


```
(func parse
      'a => (return 'a)
      x => (lambda (s k) (k s (+ x 1))) where (number? x)
      [x . xs]  => (lambda (s k)
		     (parse x s
			    (lambda (s1 x1)
			      (parse xs s1
				     (lambda (s2 xs1)
				       (k s2 [x1 . xs1]]))))))
      x => (return x))
```

定义 return

```
(defun return (x)
  (lambda (s k)
    (k s x)))
```

用 return 改写 parse:

```
(func parse
      'a => (return 'a)
      x => (lambda (s k) (k s (+ x 1))) where (number? x)
      [x . xs]  => (lambda (s k)
		     (parse x s
			    (lambda (s1 x1)
			      (parse xs s1
				     (lambda (s2 xs1)
				       (k s2 [x1 . xs1]]))))))
      x => (return x))
```

好像卡住了，怎么继续推导出：

```
(defun bind (m f)
  (lambda (s)
    ((m s)
     (lambda (s1 v)
       (f v s1)))))
```
