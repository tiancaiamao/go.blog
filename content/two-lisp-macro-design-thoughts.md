我在设计的一个 lisp 方言，实现宏的时候遇到的两个问题。这两问题其实都是影响到语言设计的，记录一下。

## pattern match 宏展开，以及尾递归

pattern match 是我想默认带进语言里面的 feature，在这个语言中默认使用 pattern match 定义函数：

```
(func acc
    n => (acc (- n 1))
    0 => 0)
```

很明显，这是一个尾递归的函数。所以我们调用 `(args 66666666)` 不应该爆栈。

func 是一个宏，展开后是用的 match 宏：

```
(defun acc (args)
    (match args
        n => (acc (- n 1))
        0 => 0))
```

所以这里对 match 宏有一个要求：**不能把原本尾递归的代码，展开之后变成非尾递归了**。

这个要求实现起来，可比听起来要复杂很多。

match 宏展开会有一个膨胀问题，假设我们的 match 条件是 

```
(or
  (and A B C)
  (and D E F)
  (and G H))
```

用最朴素的方式展开，会变成

```
(if A
    (if B
        (if C
            ;; match condition (and A B C)
            (if D
                (if E
                    (if F
                        ;; match condition (and D E F)
                        (if G
                            (if H
                                ;; match condition (and G H)
                                (error "no match")))))))
        (if ;; handle (or (and D E H) (and G H)) cases))
    (if D
        (if E
            (if F
                ;; match condition (and D E F)
                (if G
                    (if H
                        ;; match condition (and G H)
                        (error "no match")))))))
```

* 假设 match 到 `(and A B C)` 中的 A 失败了，接下来要把 `(and D E F)` 和 `(and G H)` 代码生成一遍
* 假设 match 到 `(and A B C)` 中的 B 失败了，接下来要把 `(and D E F)` 和 `(and G H)` 代码生成一遍
* 假设 match 到 `(and A B C)` 中的 C 失败了，接下来要把 `(and D E F)` 和 `(and G H)` 代码生成一遍

这种代码膨胀的是非常夸张的，会有很多很多的重复代码生成... 宏展开之后的代码完全没法读了。

避免重重代码的方式，就是把结果存到变量里面。一种写法是引入特殊的 fail 值，另一种是用 CPS 风格写代码。比如说使用 fail 值的方式

```
(lambda ()
    (if A
        (if B
            (if C
                ;; match condition (and A B C)
                (fail))
            (fail))
        (fail)))
    
(lambda ()
    (if D
        (if E
            (if F
                ;; match condition (and D E F)
                (fail))
            (fail))
        (fail)))
```

独立处理各个 pattern match 的 case，如果失败，直接返回特殊 fail 标记。然后在一个循环里面依次判断各个 match 的 case

```
(func loop
    [] -> (error "no match")
    [p1 | p2] -> (if (= (p1) (fail))
                     ;; match case p1
                     (loop p2)))
```

代码膨胀的问题就解决了，以前每个 `(fail)` 都是对应了大量的重复代码的。

CPS 方案也是类似的，不过是把 `(fail)` 变成直接调用 cc 跳转到接下来要处理的 case 的逻辑中：

```
(if A
        (if B
            (if C
                ;; match condition (and A B C)
                (cc 1))
            (cc 1))
        (cc 1))
        
cc 1 = 
(if D
      (if E
          (if F
              ;; match condition (and D E F)
              (cc 2))
          (cc 2))
      (cc 2))
```

无论是引用 fail 标记，还是用 cps 风格写法，它们都带来一个问题，就是结果不再是最终结果，需要保存下来。这可能会导致原本尾递归的代码，展开后变成不是尾递归的！

这个影响还是很大的。我原本准备在 clojure 上面实现我的 lisp 方言，但是由于 clojure 不是支持尾递归的，导致这种 pattern match 实现展开以后会爆栈。
原本的

```
(func acc
    n => (recur (- n 1))
    0 => 0)
```

在展开后 recur 会移动到其它位置，不再是尾调用的位置。

小结一个就是说，**想要 match 宏代码展开不膨胀，必定会保存中间结果，保存中间结果可能会使原本尾递归的地方变得不再尾递归。**像 clojure 里面的 recur 这种"伪"递归是不行的。

**如果想比较好的支持 match，至少要求语言层面支持严格尾递归了。**

## 宏展开，以及对特殊表处理

在设计这个语言的时候，另一个我纠结了很久的事情是，宏是否应该理解语义。

如果宏是要理解语义的，那么就要像 scheme 那样做样卫生宏，要考虑宏定义环境，和宏展开环境。这会带来非常高的实现复杂度。最后我决定做成非卫生宏。让宏是完全工作在符号层的，也就是 sexp 上面，不理解语义。

我发现实现宏的时候有两种写法。第一种是：

```
(func macroexpand
      x => (let y (compose *macros* x)
                (if (equal? x y)
                 x
                 (walk macroexpand y))))
```

walk 是一个对 sexp 每个元素进行访问的函数，compose 就是 clojure 里面的 `-->` 宏。这个函数意思是，对展开后的结果，跟展开之前对比，直到不会变化了，就是完全展开了。像是一个不动点。

另外一种写法是这样的：

```
(func macroexpand
    (list 'lambda ...) => (expand-lambda ...)
    (list 'let ...) => (expand-let ...)
    (list 'if a b c) => (expand-if ...)
    (list 'set ... => (expand-set ...)))
```

第二种写法中，对 lambda, let 等等特殊表都进行特殊处理了。我开始不理解要什么要这么做，直到后来自己实现时，才明白 **特殊表的语法，对宏展开会有影响**！ 

比如 lambda 在 scheme 里面是一个特殊表 `(lambda (max a b) body)`，假设在我设计的语言里面，宏是不理解语义的，也就是说，它并不知道 lambda 是特殊表，它会对 `(max a b)` 继续调用宏展开，而不是把它当作参数！

参数 `max` 正好是一个宏的名字，展开之后就有问题了，最后变成 `(lambda (if (> a b) a b) body)`。

我才理解，shen 语言设计的时候 lambda, let 这些都是不带括号的，它是 `(let a 3 b 5 (+ a b))` 这种，不像 scheme `(let ((a 3) (b 5)) (+ a b))`，在 shen 里面 let 就不是特殊表了。

在第一种写法里面，如果特殊表里面有 `()`，就必须特殊处理。这会让宏的实现复杂化。所以这里也算是对语言设计有影响的。我希望自己设计时，让宏完全不理解语义，甚至连特殊表都不需要理解，就更接近 lisp 原旨教义了。
