cora 语言是我设计的一门 lisp 方言。又是一门 lisp？是呀！**每个真正的 lisp 程序员都应该发明自己的语言的。**。lisp 我一直在学习，关于这门语言我思考了许多年，最终把我喜欢哪些 feature 写下来，也算作是做一个交待吧。

实现一门语言的解释器或者编译器，跟设计一门语言，其实不是同一种乐趣。实现是由别人定好了标准，自己只是按标准做出来。而设计语言的时候，需要了解许许多多语言，知道它们设计背后的思考。然后再决定，哪些 feature 是自己语言里面需要的，哪些是不需要的，这是需要更多审美。另外，**用自己设计的语言写东西，是一件非常有成就感的事情**。

在这里简单介绍一下这门语言。

cora 受到的最主要的影响来自于 scheme 和 shen。当然，考虑到我接触过的那么多语言，也不能排除一些影响来自于其它的项目，比如 femtolisp，比如 ocaml，甚至是 Go。

目前只是做了一个解释器实现，而且代码还非常糙（0.1 版本都没到，迫不急待就来写博客，向世界宣布：hello, world）。代码在这里：[https://github.com/tiancaiamao/cora](https://github.com/tiancaiamao/cora)

使用很简单，下载代码，然后 go build 生成 binary。然后 `./cora` 就会进入到 repl 了。

## 闭包是可视化的

这个是 "借鉴" femtolisp 的做法：直接在 lambda 表后面追加环境，环境用的是关联表表示的。

```
(lambda (x) x) ;; 一个闭包
(lambda (a) 3 (b . 5) (c . 7)) ;; 在 ((b . 5) (c . 7)) 环境下面的闭包
```

比如执行一下：

```
➜  cora git:(master) ./cora
1 #> ((lambda (a) (lambda (b) a)) 42)
(lambda (b) a (a . 42))

```

这么设计，跟 scheme 语法会有冲突，lambda 的 body 只能是单个 expression。如果要多个，就要按这种写法：

```
(lambda () (begin 1 2 3))
```

## 极简内核

特殊表只有 quote lambda if do macro 等非常少的几个。理论上，macro 也不是一定要做成特殊表的，只不过目前先这么实现着。

所有表达式经过完全的展开后，都会变成最后很少的几个特殊表。因为特殊表是会实现在最核心层的东西，越少越好。

像 defun let defmacro cond list 等等等这些东西都可以做成宏。

```
0 #> list
(macro exp (rcons (cdr exp)))
1 #> let
(macro exp (rewrite-let () () (cdr exp)))
2 #> or
(macro exp (if (cadr exp) true (caddr exp)))
```

甚至连 set 都是函数而不是特殊表。

```
(set 'or (macro exp (list 'if (cadr exp) true (caddr exp))))
```

我不喜欢括号过多，let 宏是用了更像 shen 那边的表示：

```
(let a 3 b 5 ...)
```

这明显比在 scheme 里面的写法要简单：

```
(let ((a 3) (b 5)) ...)
```

## first class macro

first class macro 并不是一个特意设计的语言特性，只是由于目前是解释器，并且有 macro 特殊表。所以在当前实现中宏是 first class 的。 `(macro xxx)` 可以传递给变量，可以当作返回值...

```
8 #> (set 'mylet let)
(macro exp (rewrite-let () () (cdr exp)))
9 #> (mylet a 3 b 5 (+ a b))
8
```

```
(defmacro m (exp) body)
```

实现上仅仅是给变量 m 绑定一个 macro 特殊表。

而到调用 `(m xxx)` 的时候会先 eval a，发现结果是一个宏，然后使用宏展开，再进行展开之后的 eval。

真正可以算语言设计层面的，是故意采纳了非卫生宏。卫生宏的实现更复杂，而且我感觉属于偏学术而不是真正解决实际问题的特性，因此在 cora 里面没有采用。

## 严格尾递归

cora 是要求严格尾递归的，这点跟 scheme 强调的一样。这个 feature 成为必须的理由，是来自于我在[实现宏的过程中的观察](two-lisp-macro-design-thoughts.md)。如果不支持尾递归，在做 pattern match 宏的时候很容易展开后的代码变成非尾递归的，然后爆栈。

比如这么写代码，总不能爆栈吧：

```
(func f
      0 => 42
      n => (f (- n 1)))
(f 9999999)
```

## partial apply

shen 语言是自动 partial apply 的。我在使用过之后觉得，真香！

自动 partial apply 可以避免很多无用的代码，相比于

```
(map (lambda (x) (+ x 1)) l)
```

这样子肯定是更简洁的：

```
(map (+ 1) l)
```

在 scheme 里面不必要的重复定义，如果支持 partial apply 就根本不用那么复杂。
本来用宏也可以做一些简化，像 clojure 里面有一些缩写，但是我不太喜欢 sugar 过多。

## 语言内置 pattern match

pattern match 是一个非常有用的 feature。本来可以做成库的，但是这个重要程度让我觉得，应该由语言内置。

```
(match (cons 1 2)
  42 42
  's 666
  (cons a b) a
  (list x y z) x
  (list-rest a b) b
  x x)
```

其实我[之前已经提过了](lisp-better-syntax.md)，应该 **让 pattern 匹配的对象，跟构造这个对象使用的同一种语法**。

值得一提的是 list-rest。list-rest 是将前面的 pattern 匹配到单个变量，而最后一个变量匹配剩下的链表部分。比如：

```
3 #> (match (list 1 2 3 4 5 6) (list-rest a b c d) d)
(4 5 6)
```

这里面 a b c 分别会匹配 1 2 3，而剩下的部分 d 匹配到 (4 5 6)

其它的理解起来都很简单，`42` 匹配常量；`(quote s)` 匹配符号 `s`； `(cons a b)` 匹配一个 cons，`(list x y z)` 匹配 3 个元素的链表；上面的表达式最后会返回 1。

所有模式都是可组合的，比如这种 `(cons (list a 1) 'xx)`

## func 宏

cora 鼓励优先使用 func 来定义函数。func 是一个宏，这个 syntax 明显是受了 Go 的启发，使用是这样子：

```
(func filter-h
      res fn () => (reverse res)
      res fn (cons hd tl) => (filter-h (cons hd res) fn tl) where (fn hd)
      res fn (cons _ tl) => (filter-h res fn tl))
```

`=>` 前面是模式，匹配函数的输入，后面是执行的操作。可以带 `where` 过滤条件。

用 pattern match 写出来比 defun 会好读一些。

```
(defun filter-h (res fn l)
  (if (cons? l)
      (if (fn (car l))
          (filter-h (cons (car l) res) fn (cdr l))
          (filter-h res fn (cdr l)))
      (reverse res)))
```

好啦，下面才是正经的介绍了：lisp 基础，可以在 cora 里面练习一下。

TL;DR

数字，字符串，布尔的 true 和 false 是常量，常量求值得到自身：

```
4 #> 1
1
5 #> "asdf"
"asdf"
6 #> true
true
7 #> false
false
```

quote 特殊表，缩写是单引号 '，被 quote 的东西不求值，可以用它得到符号：

```
8 #> 'asdf
asdf
9 #> (quote asd)
asd
10 #> (quote (a b c))
(a b c)
```

变量，在 lambda 或者 let 这些东西里面，参数都是变量：

```
11 #> (lambda (a) a)
(lambda (a) a)   ;; 这里面的 a
12 #> (let a 3 b 5 (+ a b))  ;; 这个的 a 和 b
8
```

if 表达式：

```
13 #> (if true 1 2) 
1
```

lambda 表达式，也就是定义函数：

```
17 #> (lambda (x) x)
(lambda (x) x)
18 #> ((lambda (x) x) 42)
42
```

set 是一个函数，它接受的参数是一个符号和一个值，即设置符号绑定的值：

```
14 #> (set 'a 3)
3
15 #> (set 'b 5)
5
16 #> (+ a b)
8
```

注意这个绑定是全局绑定的，跟变量不太一样。定义函数实际上就是为符号，绑定一个 lambda 表达式，比如：

```
(defun id (x) x)
```

等价于

```
(set 'id (lambda (x) x))
```

可以验证一下：

```
19 #> (defun id (x) x)
(lambda (x) x)
20 #> id
(lambda (x) x)
21 #> (set 'id (lambda (x) x))
(lambda (x) x)
```

函数调用

```
22 #> (id 42)
42
```

相互递归定义也是可以的

```
23 #> (defun even (x)
  (if (= x 0)
      true
      (odd (- x 1))))
(lambda (x) (if (= x 0) true (odd (- x 1))))
24 #> (defun odd (x)
  (if (= x 1)
      true
      (even (- x 1))))
(lambda (x) (if (= x 1) true (even (- x 1))))
25 #> (even 30)
true
26 #> (odd 31)
true
```

完。
