宏在lisp里面是一柄双刃剑，我一般是能避免使用尽量避免的，也在一直思考有没有什么能替代它。上次看到了fexpr，研究了一下，知道了fexpr和宏的区别。它们都不会对参数求值，都是把参数以原始的list形式传递。接着，它们都会调用自己的定义，对list进行展开，这一步也是相同的。最后一步才是区别：宏会对展开以后生成的表达式求值，而fexpr直接返回展开的结果。

这个设定使得fexpr比宏更加动态，使语言无法静态分析，编译期更加模糊，或者说，编译器跟运行期搅和更严重，无法脱离eval。所以似乎也不是一个我想要的东西。

如果没有宏，函数的极限在哪里？

以实现pattern match为例，我想这样写：

    (pmatch x
            ((:cons a b) (+ a b))
            ((:nil) 42))

如果x是一个cons，pattern match会将a绑定到第一个，将b绑定到第二个，然后执行`(+ a b)`。没有宏的情况下，最接近能做到这个程度：

    (pmatch (cons 1 2)
            (:cons (lambda (a b) (+ a b)))
            (:nil (lambda () 42)))

如果判断是cons，则在一个函数里面计算`(+ a b)`，则`:cons`负责提供函数的参数，a绑定到第一个，b绑定到第二个。

先摆最终代码，别看代码不长，这可花了我一晚上才写出来。

    (define (selector is? . get)
      (lambda (f)
        (lambda (x)
          (lambda (cc)
            (if (is? x)
                (apply f (map (lambda (f) (f x)) get))
                (cc x))))))

    (define pmatch
      (lambda (x . sel)
        (if (null? sel)
            (error 'panic)
            (((car sel) x)
            (lambda (x0)
              (apply pmatch (cons x0 (cdr sel))))))))

    (define :cons (selector pair? car cdr))
    (define :nil (selector null?))

-------------------------------------------------------------

TL;DR

lisp是高度抽象的，代码很短，背后的思维却异常的复杂。如果别人不告诉你怎么推出来的，代码跟本看不懂。

首先，pmatch函数的参数，应试是一个值，后面跟若干个函数，后面的函数依次对传值x进行计算，如果通过了就结束，没通过就继续尝试下一个。

    (pmatch (cons 1 2) sel1 sel2)

然后想sel1，sel2展开以后，应该是类似于这样子：

    (lambda (x)
      (if (pair? x)
          ((lambda (a b) (+ a b)) (car x) (cdr x))
          ?))
    (lambda (x)
      (if (null? x)
          ((lambda () 42))
          ?))

由于要依次比较，我们要做一个CPS变换(这是个重点)，得到

    (lambda (cc)
      (lambda (x)
        (if (pair? x)
            ((lambda (a b) (+ a b)) (car x) (cdr x))
            (cc x))))

完整的写下来：

    (lambda (x)
      (let* ((:cons (lambda (cc)
                      (lambda (x)
                        (if (pair? x)
                            (      (lambda (a b) (+ a b))     (car x) (cdr x))
                            (cc x)))))
            (:nil (lambda (cc)
                    (lambda (x)
                      (if (null? x)
                          (       (lambda () 42)    )
                          (cc x)))))
            (cc0 (lambda (_) (error 'panic)))
            (cc1 (:nil cc0))
            (cc2 (:cons cc1)))
        (cc2 x)))

验证一下，拿它个调用`(cons 1 2)`得到3，调用`'()`得到42，好，继续往下。

    (lambda (x)
      (let* ((:cons (lambda (cc)
                      (lambda (x)
                        (if (pair? x)
                            (      ????     (car x) (cdr x))
                            (cc x)))))
            (:nil (lambda (cc)
                    (lambda (x)
                      (if (null? x)
                          (       ????    )
                          (cc x)))))
            (cc0 (lambda (_) (error 'panic)))
            (cc1 (:nil cc0))
            (cc2 (:cons cc1)))
        (cc2 x)))

`????`是要作为参数传进去的，我们提取成参数`f`和`g`，再交换一下参数顺序，就变成了：

    (let* ((:cons (lambda (f)
                    (lambda (x)
                      (lambda (cc)

                        (if (pair? x)
                            (f (car x) (cdr x))
                            (cc x)))
                      )))
          (f (lambda (a b) (+ a b)))
          (:nil (lambda (f)
                  (lambda (x)
                    (lambda (cc)

                      (if (null? x)
                          (f)
                          (cc x)))
                    )))
          (g (lambda () 42))

          (cc0 (lambda (_) (error 'panic)))
          (cc1 (lambda (x) (((:nil g) x) cc0)))
          (cc2 (lambda (x) (((:cons f) x) cc1))))

      (cc2 x))

上面的`:cons`和`:nil`是可以单独拿出来的：

    (define :nil
      (lambda (f)
        (lambda (x)
          (lambda (cc)
            (if (null? x)
                (f)
                (cc x)))
          )))

    (define :cons
      (lambda (f)
        (lambda (x)
          (lambda (cc)

            (if (pair? x)
                (f (car x) (cdr x))
                (cc x)))
          )))

接下来又是个重点，`:nil`和`:cons`结构是比较类似的，我希望只保留必须的代码，尽量不要重复。

必要的信息包括判断是否是某个类型`pair?`，`null?`，以及取结构里面某个域`car`，`cdr`。

    (define :cons (selector pair? car cdr))
    (define :cons
      (lambda (f)
        (lambda (x)
          (lambda (cc)

            (if (pair? x)
                (f (car x) (cdr x))
                (cc x)))
          )))

需要让两者是等价的。那么如何定义`selector`呢？

    (define (selector pair? car cdr)
      (lambda (f)
        (lambda (x)
          (lambda (cc)
            (if (pair? x)
                (f (car x) (cdr x))
                (cc x))))))

    (define (selector nil?)
      (lambda (f)
        (lambda (x)
          (lambda (cc)
            (if (nil? x)
                (f)
                (cc x))))))

于是抽象出`selector`的定义：

    (define (selector is? . get)
      (lambda (f)
        (lambda (x)
          (lambda (cc)
            (if (is? x)
                (apply f (map (lambda (f) (f x)) get))
                (cc x))))))

接下来看`pmatch`，它应该是这样子的：

    (define pmatch
      (lambda (x sel1 sel2)
        (let* ((cc0 (lambda (_) (error 'panic)))
              (cc1 (lambda (x) ((sel2 x) cc0)))
              (cc2 (lambda (x) ((sel1 x) cc1))))
          (cc2 x))))

其中`sel1`和`sel2`分别是：

    (:cons (lambda (a b) (+ a b)))
    (:nil (lambda () 42))

写成let不太方便，如果把pmatch的let展开，就是:

    (define pmatch
      (lambda (x sel1 sel2)
        ((sel1 x)
        (lambda (x)
          ((sel2 x)
            (lambda (_)
              (error 'panic)))))))

最后为了处理许多的selector，把参数做变成长的：

    (define pmatch
      (lambda (x . sel)
        (if (null? sel)
            (error 'panic)
            (((car sel) x)
            (lambda (x0)
              (apply pmatch (cons x0 (cdr sel))))))))

推导结束，以上！

-------------------------------------------------------------------------

再放几个好玩的，比如说定义option类型

    (define :some (selector vector? (lambda (x) (vector-ref x 0))))
    (define :none (selector null?))
    (pmatch #(5)
            (:some (lambda (x) x))
            (:none (lambda () 42)))

再比如实现else：

    (define :else (selector (lambda (x) #t) (lambda (x) x)))
    (pmatch '(a b c)
            (:cons (lambda (a b) (+ a b)))
            (:nil (lambda () 42))
            (:else (lambda (x) x)))
