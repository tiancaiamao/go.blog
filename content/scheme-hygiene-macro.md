本文面向的阅读对象是使用过 scheme 的卫生宏，希望了解它是如何实现的，或者在实现自己的 lisp 语言的一类人。
另一个基础要求是，至少知道动态作用域和静态作用域。

## 宏的分类

scheme 语言里面，宏是一个很让人误解的话题，它使用是 syntax-rules。其实 syntax-rules 是混杂了两个概念，所以使它的理解变得复杂。应该拆成两个维度来理解。一个维度是卫生/非卫生。另一个维度是过程宏/高级宏。

先说过程宏和高级宏维度。一些其它的 lisp 或者 common lisp 里面，有 defmacro，这是很直观的，宏就类似于一个普通函数，它的是输入参数的 sexp，然后输出也是 sexp。这个宏函数里面具体实现了如何变换的过程，所以是过程宏。
在 scheme 的 syntax-rules 里面，它是使用了模式匹配的方式，来描述如何将一个 sexp 变换成另一个 sexp。这就是高级宏，或者说模式匹配宏。

再来说卫生宏。卫生或者不卫生，跟高级宏过程宏其实是不相关的概念。如果用两个维度来分类，那么 syntax-rules 是高级宏，是卫生宏。而一些 lisp 的 defmacro 是非卫生宏，是过程宏。
还有另外一些系统，是卫生的，却是过程宏。

    defmacro	过程宏	不卫生
    syntax-rules	高级宏	卫生
    syntatic-closure	过程宏	可实现卫生
    explicit-renaming	过程宏	可实现卫生
    syntax-case	可过程或高级	可实现卫生

## 什么是卫生

**卫生或者不卫生的本质问题，还是作用域问题**，理解这个了，对于卫生宏就理解了。

让我们先看，defmacro 存在什么问题呢？

    (defmacro (swap! A B)
        (let ((tmp ,A))
              (set! ,A ,B)
              (set! ,B tmp)))
              
    (swap! tmp x)

展开之后，变成了

    (let ((tmp tmp))
         (set! tmp x)
         (set! x tmp))
         
这里是一个变量重名问题，宏里面引入的 tmp，跟宏调用的参数 tmp 相互冲突了。有经验的 lisp 程序员会说，应该使用 `(gemsym tmp)`，然而，这并不是卫生的。也并不能解决更多问题。

假设用户这么玩，定义宏 test：

    (define x 'hello)
    (defmacro (test)
        '(print x))

调用宏展开

    (let ((x 'world))
        (test****
        
得到的结果是 hello 还是 world 呢？这个点就能体现卫生宏和非卫生宏的本质区别了。一个符号 x，它在宏定义的位置是代表的什么含义，以及它在宏展开的位置代表的是什么含义。**非卫生宏是做 sexp 的无脑替换。卫生宏是需要理解语义的**。

这个问题非常类似于静态作用域和动态作用域的问题。函数定义：

    (define x 3)
    (define (f) x)
    
函数调用：

    (let ((x 5))
        (f))
        
在函数定义的作用域里面，x 的值是 3。在函数调用的时候，作用域里面有一个变量 x 对应的值是 5。如果一门语言采用动态作用域，调用 f 返回值将会是 5，因为动态作用域用函数使用时的上下文决定一个变量的值。在静态作用域里面，f 返回值是 3。

这里要强调的一个概念是“引用透明性”。静态作用域好处就在于，不管 f 在哪里被调用(引用)，它的结果都是确定的，因此不会有意想不到的 bug。
前面说了，卫生或者不卫生的本质问题，还是作用域问题。宏要处理一系列符号，宏又有宏定义阶段和宏展开阶段，卫生问题说白了就是哪些符号是对应在宏定义阶段，哪些符号是对应在宏展开阶段。如果宏是卫生的，它就具有“引用透明性”，定义阶段就确切知道每个符号的意义，不会出现意外的绑定。

## 历史算法

历史上比较有影响力的几个算法，下面是一个简单的时间线。

* 1986: Kohlbecker - introduced the idea of hygiene, low-level, used an O(n^2) coloring algorithm
* 1987: Kohlbecker - introduced declare-syntax, high-level, the precursor to syntax-rules
* 1988: Bawden & Rees - "Syntactic closures," low-level, faster than Kohlbecker's algorithm
* 1991: Clinger & Rees - Explicit renaming, low-level, based on syntactic-closures but also supports syntax-rules
* 1992: Dybvig - Syntax-case, primary motivation to remove the distinction between low-level and high-level

一个一个解释。第一个，KFFD，Kohlbecker 在 86 年的一篇 paper 里面首次提出了宏的卫生的概念。四位作者的首字符缩写出来，就是 KFFD。具体实现方式跳过了，这个算法低效，并且现今已经没有人在用了。

第二个，syntax-rules。作者还是 Kohlbecker，在 87 年的一篇 paper 里面，提出了用模式匹配的方式来声明宏的 idea。这就是 syntax-rules 的前身了。

第三个，syntax closure。开始重点介绍。既然我们知道了卫生宏的本质问题，符号到底对应宏定义时期的环境，还是宏展开时期的环境，我们可以添加两个参数：

    (define-syntax foo
        (lambda (form usage-environment macro-environment)
            ...))
            
在 macro 里面显示的指定，每个符号是对应定义时期，还是展开时期，这样就不会出现误解了。再可以简化一下，把宏定义期环境作为默认值，就可以去掉 `macro-environment` 参数，于是得到 `sc-macro-transformer`：

    (define-syntax swap! 
      (sc-macro-transformer 
        (lambda (form env) 
          (let ((a (make-syntactic-closure env '() (cadr form))) 
                (b (make-syntactic-closure env '() (caddr form)))) 
            `(let ((value ,a)) 
              (set! ,a ,b) 
              (set! ,b value)))))) 

这里面的 let value set! 等等都是在宏的定义期环境里面的符号，因此不会跟运行期环境里面的符号冲突。而 a 和 b 这两个符号，则是 swap! 宏里面指定了使用宏展开时期的环境。

第四个，通过 explicit renaming，来决定卫生问题。`er-macro-transformer` 跟 `sc-macro-transformer` 在使用方面看起来只是一个提供的是环境，另一个提供的是函数，由 rename 函数决定了使用宏定义时的环境。

    (define-syntax swap! 
      (er-macro-transformer
        (lambda (form rename compare) 
          (let ((a (cadr form))
                (b (caddr form)))
            `(,(rename 'let) ((,(rename 'value) ,a))
                (,(rename 'set!) ,a ,b)
                (,(rename 'set!) ,b ,(rename 'value)))))))

syntatic closure 和 explicit renaming，符合那些 defmacro 的人的使用习惯，如果把 `rename` 当成 `gensym` 来用，但它是可以实现成卫生的。背后的算法还是区分了宏定义环境和宏展开环境，不是简单的重命名。

第五个要说的，是 syntax-case。syntax-case 是在 r6rs 以及 chez-scheme 里面实现的，作者也正是 chez 编译器的作者 Dybvig。syntax-case 非常强大，既可以支持高级宏，也可以支持过程宏，并且可以处理卫生和不卫生。
但是强大的代价是这个宏系统非常复杂，理解，使用和实现上面都是。

最后最后提一下，当前最先进的(截止2018年)应该是 racket 和 gerbil 的宏系统。使用的实现的是用的 [Scope Set](http://www.cs.utah.edu/plt/scope-sets/index.html)。这个 matthew flatt 在 2012 年就出 paper 了。
在 racket 里面，引入了语法对象 syntax。为 syntax 绑定了 scope，多了一层抽象来处理宏。
这不只是记法问题。本来有 symbol。决定 symbol 要区分是 variable 或是 identifier 又需要 quote。我觉得有越搞越复杂的倾向。有点脱离了 sexp 这些简单的初衷。

由这些个算法的发展历史小结一下，这个问题在研究上基本已经到头了。目前并没有什么既简单，又强大的方案出来。syntax-case 很强大但复杂。我觉得如果是自己实现，可能 syntatic closure 或者 explicit renaming 会[好一点](http://mumble.net/~jar/pubs/scheme-of-things/easy-macros.pdf)。

## 更多思考

### 宏的展开次序问题

宏展开是一个递归展开的过程，那么展开的顺序是一个需要考虑的问题。这在语言标准里面是没定义的。另外，如果宏带副作用的，整个结果就不确定了。导致宏不是一个可组合的东西。

### 编译期展开和运行期展开

完全展开和非完成展开，解释器可以实现成运行期展开，每遇到宏就进行展开了再解释。编译器必须要求宏是编译期完成展开的。需要一个 expander 递归的处理宏展开的过程，直到这个过程结束。注意，过程宏是可以在展开时期做计算的，整个宏展开过程就不保证收敛了。

### 宏展开过程中引入的作用域

宏调用宏，展开的过程中，会引入新的宏展开作用域，也是卫生需要考虑到的。

### 过程宏的问题，为什么r7rs r5rs 标准里面都是用的 syntax-rule

过程宏很难标准化。主要是它把编译器和运行期的耦合弄得不确定了。前面说过，卫生的宏展开，是要考虑语义的。而考虑语义的东西，有些在运行期才能决定。

    (let ((x 3))
        (macro-test (if x > 5))
        
比如过程宏根据某条件做展开，条件要在运行期计算才确定。宏展开期间它前不知道一些运行期的条件，这就无法完全编译期展开了。
syntax-rule 的一个好处，它可以避免过程宏的这种问题，写出来的宏是可以完全展开的。

### 跟模块，separate compilation 的关系

宏跟模块有莫大的关联性，racket 强大在 module，这也是为什么它需要采用复杂的 macro 系统来支持 module 的实现。

宏可以在展开时计算一些东西，计算依赖的函数本来是运行期的，被依赖到了编译器。

有了 module 和 separate compilation 之后，其个宏要引入其它模块函数，而在使用这个模块的宏，另外模块还没有编译，这里面的依赖就会有些难搞。

## 参考资料

[macro systems and chicken](https://lists.gnu.org/archive/html/chicken-users/2008-04/msg00013.html)
[评论区 groovy2shoes 的回复](https://news.ycombinator.com/item?id=15394603)
[Implementing Lexically Scoped Macros](http://mumble.net/~jar/pubs/scheme-of-things/easy-macros.pdf)
[Binding as Sets of Scopes](http://www.cs.utah.edu/plt/scope-sets/index.html)
