[接上篇](/scheme-hygiene-macro.md)，前面对 scheme 卫生宏的实现方式有个整体的介绍，这一次具体讲其中 explicit renaming 这种方式的实现原理。

首先讲 [alpha 变换](https://en.wikipedia.org/wiki/Lambda_calculus#%CE%B1-conversion)。alpha 变换这东西，说的就是函数的参数名字其实是无所谓的。

    (lambda (a b) (+ a b))

跟下面这个完全是等价的。

    (lambda (a1 b2) (+ a1 b2))
    
alpha 变换就是自由的把函数名字随便换。alpha 变换这么实现：

    (define (alpha-convert exp env)
        (match
            (x (symbol? exp)) (replace-symbol x env))
            (lambda (x) body)
              (let ((new-env (cons (x . x1) env)))
                `(lambda (x)
                    ,(alpha-convert body new-env))))
                    
用一个环境，环境里面是 `(原符号 . rename后的符号)`。

为什么要讲 alpha 变换呢？ 因为 explicit renaming 其核心就是在做 alpha 变换！

考虑一个宏：

    (swap! x y)
    (let ((tmp x))
          (set! x y)
          (set! y tmp))

如果我们把展开宏显式的重命名一下，注意前面说过的卫生的原则 -- 卫生的本质问题，还是作用域。把需要使用 宏定义时作用域 的变量用 [] 框起来：

    ([let] (([tmp] x))
          ([set!] x y)
          ([set!] y [tmp]))

[let] [set!] [tmp] 指代的是宏定义时的符号，而 x y 则是宏展开时期的符号。每一次宏展开生成都赋予一个唯一 id，因此我们可以把上面写成：

    ([let 1] (([tmp 1] x))
          ([set! 1] x y)
          ([set! 1] y [tmp 1]))
          
如果有多次的宏展开，比如 (swap! (or x 1) y) 第一次展开 swap! 时：

    ([let 1] (([tmp 1] (or x 1))
          ([set! 1] (or x 1) y)
          ([set! 1] y [tmp 1]))
          
第二次再展开 or 之后：

    ([let 1] (([tmp 1] ([if 2] x x 1))
          ([set! 1] ([if 2] x x 1) y)
          ([set! 1] y [tmp 1]))
          
注意其中的 [let 1] 跟 [if 2] 分别是对 swap! 和 or 的两次展开过程，每次展开都对应了一个唯一 id。如果我们有一个 [let 1] 和 [let 3]，它俩有可能是同一个东西，只是在不同的宏展开过程中赋予的 id 不同。

如果看 `er-macro-transform` 的说明，它说每次展开都是在不同的词法环境中，所以不同的宏展开后的符号不会跟其它任何地方冲突。怎么理解的呢？嗯，其实就是做了 alpha 变换。对于前面的前面的例子，如果我们再夹杂一个 alpha 变换，可以写成这样子：

    ([let 1] (([tmp 1] x@523423132)
          ([set! 1] x@523423132 y@44512342)
          ([set! 1] y@44512342 [tmp 1]))

alpha 变换使用的 宏展开时环境： ((x . x@523423132) (y . y@44512342))

这是最重点的地方，准确来说，遇到宏展开会做两步操作：

* 第一步是在宏展开时，把 let set! 变成 [let 1] [set! 1]
* 第二步是将展开的结果做 alpha 替换，对普通的符号，使用宏展开时环境替换；对于 [let 1] 这种，使用宏定义时环境替换

或者放点代码会更清楚一点：

    (define (expand exp menv env)
      (cond
      ((symbol? exp) (alpha-convert exp env))
      ((generated? exp)
        (let ((env-of-def (assq (generated-uid exp) menv)))
          (alpha-convert (generated-sym exp) env)))
      ((pair? exp)
        (let ((den (binding (car exp) menv env)))
          (cond
          ((special? den) (expand-special-form den exp menv env))
          ((macro? den) (expand-macro den exp menv env))
          (else (expand-application exp menv env)))))
      (else exp)))  ;; for const like string, number and so on
    
其中的 env 是一个符号，到一个绑定。绑定内容可能是 alpha 变换后的符号，或者特殊表 if begin set! lambda，也可能是宏。

宏的表示是 转换函数，以及宏定义时环境。generated 就是 (name . uid)

menv 是 uid 到 env 的映射，因为展开 generated 需要两步，通过 generated-uid 从 menv 获取到 generated 定义时的环境，下一步在是在该环境里面对 generated-sym 做 alpha 变换。
    
    (define (expand-macro mac exp menv env)
      (let* ((transform (macro-func mac)) ;;提取宏转换函数
            (env-of-def (macro-env mac)) ;;提取宏定义时环境
            (uid (unique-id))            ;;每次展开生成唯一 id
            (new-menv (cons (uid . env-of-def) menv)) ;;唯一 id 到 env 绑定
            (rename (lambda (x) (make-generated x uid))) ;; rename 会传递给宏用于绑定 宏定义时环境
            (new-exp (transform exp rename))) ;; 调用宏转换函数，会生成 [let uid]
        (expand new-exp new-menv env))) ;; 对宏展开的结果，递归再展开

expand-special-form 是对 if begin set! lambda 等东西做展开，其中 lambda 的展开：

    (define (expand-lambda exp menv env)
      (let* ((args (cadr exp))              ;; (a b c)
            (values (map rename args))     ;; (a@5 b@6 c@7)
            (binds (map cons args values)) ;; ((a . a@5) (b . b@6) (c . c@7))
            (new-env (append binds env)))  ;; 新的 env，用于 alpha 变换
        `(lambda ,values
          ,@(expand-sequence (cddr exp) menv new-env))))

expand-special-forma 里面另个比较特殊的是 expand-defmacro，它要将 mac 加到 env 里面去。
应该解释清楚了。

---------

最后看一个问题是，为什么 alpha 变换是卫生的，而 gensym 不是？

    (let ((tmp#jsdjflsdf 32))
        (some-macro))

在 some-macro 展开用 `(let ((tmp (gensym))))` 引入的 tmp，可能正好是 `tmp#jsdjflsdf`，就会被前面的绑定误捕获了。而在 alpha 变换中，`tmp#jsdjflsdf` 在环境里面是 `tmp#jsdjflsdf@123`，宏展开时的 id 不可能是 `@123`，它只能是一个不一样的 id 了。
