### lisp 缺一个好的语法

满世界的人都对全是括号的表示充满讨厌，我也讨厌。有人说，lisp 有语法么？ lisp 的标准语法不是 S表达式么。极端分子甚至强调 S表达式就是世界上最好的语法，括号是它精华的东西，认为搞些其它花里胡哨的语法偏离了教义。

先打脸

    (defun map
        F []		-> []
        F [X | Xs]	-> [(F X) | (map F Xs)])
        
这种写法不比下面这种优雅得多？ 不要拿模式匹配说事，模式匹配的问题后面讲。

    (define map
        (lambda (f l)
            (if (null? l)
                '()
                (cons (f (car l))
                      (map f (cdr l))))))

S表达式里面，只有两种东西，一种是 symbol，一种是 cons。然而实际上语言都会支持各种原子类型的符号，比如 number，比如字符串。

遇到自定义类型的时候，就有一点欠缺了，定义结构体什么的，纯用 S表达式就有点别扭。
为什么不用 `#R"(?<=95|98|NT|2000)Windows"` 来表示正则？ 或者是自定义用来表示结构体的语法 `T{A : 3, B : "sd"}`。一旦这些东西多起来之后，S表达式就不那么纯粹了。

### reader 宏很有意义，自定义 reader 收获更大

其实 lisp 是有 reader 宏的，像单引号的 quote，反引号的 quasiquote，逗号的 unquote，这些都是内置的 reader 宏。reader 在遇到任何这些特殊符号的时候，就自动改写S表达式了。`(quote s)` 的等价写法 `'s`。

reader 宏是很有意义的，比如我们遇到 `{` 就按我们自己定义的方式来解析，最后输出的还是 S表达式。既然能够接受 reader 宏，为什么不更推进一步，自定义整个 reader 呢？

自定义 reader 之后，语法喜欢什么口味都可以自己设计，比如即使变成这样子，都是完全合理的，反正过了 reader 之后，就是正统的 S表达式了：

    func f() {
    }
    
**不用关心真正的语法，可以用任何自己喜欢的表示，自己写 reader，然后转换成 S表达式。**

### quote 的问题

quote 是一个 reader 宏。在[知乎回答问题](https://www.zhihu.com/question/60702229/answer/213029957)的时候，提到过 quote。

> quote 存在的真正意义就是，让用户可以选择到底是代码还是数据，但它只是实现的途径之一，但并不是必须的。

在 shen 语言里面，一个符号求值后得到的是它自身，不需要使用 quote。为了区分变量和符号，求值规则就有点特殊了，要取决于上下文，有绑定就是变量，没有绑定就是符号：

`(lambda (x) x y)` 在这个 lambda 里面， `x` 是变量， `y` 是符号

即使不使用 shen 这么特殊的规则，要生成符号也完全可以使用其它方式，比如 `(intern "x")`。只要有办法制造符号，制造 cons，就可以制造 S表达式。任何语言，只要写一个 reader，制造 S表达式还不是很简单的事情？
那岂不是所有语言都可以是 lisp 了？ 那我们 lisp 党的迷之优越岂不是荡然无存了？

扯了半天废话，我是要说什么来着，哦， quote 没毛卵用，并不是 lisp 里面必须的。

### 嵌套反引用有害

直接[帖个链接](http://xlambda.com/blog/2013/03/10/nested-backquotes-considered-harmful/)，不解释了。

为了解决嵌套反引用的问题，我提出(其实不是我提出的，shen 语言是这么干的)一个更好的语法表示：中括号的表示不求值，圆括号表示需要求值。

    [A (f B [C D])]

不论嵌套多少层，仍然是一目了然。

### pattern match 的 pattern 语法

不管是 syntax-rules，还是 pmatch 里面，pattern 的写法都让人感到很不一致，像是不在同一门语言里面。
举一个例子：

    (pmatch exp
          [,x (guard (symbol? x)) x]
          [(,M ,N) `(,(compile-bc M) ,(compile-bc N))]
          [(lambda (,x) ,y) (guard (eq? x y)) `I]
          [(lambda (,x) (,M ,y))
          (guard (eq? x y) (not (occur-free? x M))) (compile-bc M)]
          [(lambda (,x) (,M ,N)) (guard (and (not (occur-free? x M)) (occur-free? x N)))
          `((B ,(compile-bc M)) ,(compile-bc `(lambda (,x) ,N)))]
          [(lambda (,x) (,M ,N)) (guard (and (occur-free? x M) (not (occur-free? x N))))
          `((C ,(compile-bc `(lambda (,x) ,M))) ,(compile-bc N))]
          [(lambda (,x) (,M ,N)) (guard (or (occur-free? x M) (occur-free? x N)))
          `((S ,(compile-bc `(lambda (,x) ,M))) ,(compile-bc `(lambda (,x) ,N)))]
          [(lambda (,x) ,M) (guard (not (occur-free? x M))) `(K ,(compile-bc M))]
          [(lambda (,x) ,M) (guard (occur-free? x M))
          (compile-bc `(lambda (,x) ,(compile-bc M)))]**

注意观察到，匹配符号用的符号本身，匹配变量就需要加一个逗号。
我认为不是很好，最符合直觉的方式，应该**让 pattern 匹配的对象，跟构造这个对象使用的同一种语法**。比如:

- pattern 是一个数字 42，那它应该匹配到的也是 42; 
- pattern 是一个字符串 "xyz"，那它匹配的也是一个字符串 "xyz"; 
- 构造一个 symbol 的方式是 (quote s)，缩写是 `'s`，那以匹配符号 s 的 pattern 也应该是 `'s`;
- pattern 是 `(cons X Y)`，它用于构造一个 cons，那么匹配的也应该是一个 cons;
- x 就直接匹配任何变量了

还记得前面说过自定义 reader，在自定义的 reader 里面，构造 list 的方式是使用中括号 []。然后

    [a b c]
    
经过 reader 转换成 S表达式以后，变成

    (cons a (cons b (cons c nil)))
    
注意到没，pattern 匹配的对象，跟构造对象使用的是同种语法表示！整个语言就更一致了。

构造一个 symbol 是使用 `(quote s)`，那么模式匹配中也使用 `(quote s)`。
假设我们自定义结构体的语法，比如 `T{A, B}`，那这个东西当 pattern 表示时，也应该匹配结构体 T 对象。

最后看一个，`(quote (a b c))` 的含义是啥？ 它不是一个构造链表的函数，不应该使用这种东西放在模式匹配里面。
即使没有 quote，假设构造符号使用的是 `(intern "xxx")`，那模式匹配的写法也应该是 `(intern "xxx")`。

为了避免 `(quote (a b c))` 这种奇怪的东西出现在语言里面，最好的办法是，语言里面只允许使用 `(quote symbol)`，不能 quote 其它的。

好啦，看个例子：

    (match v
        x		-> (+ x 2)	;; 模式匹配一个变量
        []		-> #t	;; #t 和 #f 和 [] 都是基本对象
        [a b]	-> a	;; [a b] 其实是 (cons a (cons b []))
        [a | b]	-> a	;; [a | b] 等价于 (cons a b)
        T{a}	-> a	;; 如果有自定义结构体
        'a		-> 'b	;; 匹配一个符号， 'a 等价于 (quote a)
        _		-> (error "nothing")) ;; 下划线跟变量差不多

定义函数也是使用模式匹配的，跟 match 一样：

    (defun f
    	a b -> 42	;; 跟 match 不同的是这里可以多参数
        ['lambda x] y -> y)
        
跟 shen 语言不同的是，不使用大小写区分变量和符号，符号使用 quote
