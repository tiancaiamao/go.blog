Shen语言号称是 write once, run anywhere。怎么做的呢？并是不将源代码编译成汇编或者二进制，而是编译到一个更简单的叫做[klambda](https://github.com/Shen-Language/wiki/wiki/KLambda)的语言，以下简称kl。kl扮演了一个虚拟机或者平台无关层的角色，但比较有意思的是：它是一个lisp语言。

kl是一个非常精简的lisp，几乎接近于lambda演算，大概有50多个系统函数，包括一些IO相关的。klambda存在的主要意义就是方便port，目前Shen语言已经有common lisp，elisp，C，F#，haskell，scheme等等很多的port了。Shen语言可以完全用kl实现，虽然Shen是用Shen语言写的，但它实现是将Shen语言编译到kl，运行在kl，编译到kl，如此完成自举。

kl比scheme还要精简许多。回头想想，scheme还是太复杂了，像`call/cc`的连续，卫生宏，这些其实都是很复杂的不好实现的feature。Shen暂时还没有Go的实现，想了解下kl的特性，所以用Go语言写了一个解释器。其实对于写过lisp解释器的人都是老生常谈的东西。

## eval/apply

最基本的就是`eval(exp, env)`，常量直接返回；符号就去环境里面查找定义；调用就是将实参和形参绑定，在新的环境里面apply闭包；lambda就将表达式，参数和环境一起保存起来，生成闭包。

## lisp1 vs lisp2

kl是类似lisp2的，它有一点很特殊的地方：符号求值得到的是符号本身：

    (4-) aaa
    aaa

我比较熟习scheme，但是scheme是lisp1的。实现这个feature的时候我玩了一点trick：对符号求值的时候，先到环境里面找，找得到就当变量，找不到直接返回该符号。

    ((lambda x x) 1)  // 返回1，因为x被当作变量
    ((lambda x y) 1)  // 返回符号y，因为y在环境里面找不到，当符号处理

然后函数和变量用了两个不同的namespace，其实也就是hash表啦。

    (set *v* (lambda x x))
    (value *v* 3)

变量使用变量的hash表，而函数使用函数的hash表。

    (defun f (x) (+ x 1))

变量就用set和value，函数定义就用defun。像比如

    (value f)
    (*v* 1)

都会出错，混着用都是不行的。

## partial apply和自动柯里化

kl语言规范要求，它必须是支持partial apply的。也就是说：

    (5-) (+)
    #<procedure>

函数+本来需要两个参数，如果不提供，它会返回一个partial的函数。支持partial是很好的，比如可以这样写：

    (0-) (map (+ 1) [1 2 3])
    [2 3 4]

而不用像scheme那样写成：

    > (map (lambda (x) (+ x 1)) '(1 2 3))
    (2 3 4)

怎么实现呢？其实很容易，判断一个参数是否足够，如果不够，当前的部分参数保存到环境中，生成一个新的闭包返回。当这个闭包下次有足够参数了，再被调用的时候，就继续完成运算。比如

    (defun (add x y) (+ x y))

发现参数不够，自动生成一个闭包返回：

    (add x) => (lambda (y) (+ x y))

由解释器自动改写就行了。

## 实现尾递归

kl语言规范要求，必须实现尾递归优化，即执行尾调用的时候函数栈不会增长。Go语言是不支持尾递归的，在一门不支持尾递归的语言里面，实现这个feature需要一点技巧，这个技巧叫做蹦床(trampoline)。

先看一个例子：

    (eval
      (eval
        (eval
          (eval
            (eval
              (eval

虽然Go语言有分段栈，但仍然是有大小限制的。如果是这样递归下去，栈就暴掉了。一般函数式语言才要求尾递归优化，因为函数式语言里面递归太重要了，并且喜欢用递归来实现循环。

蹦床是这样一个技术：

    func eval(ctl Control) {
         ...
         ctl.Finish = false
         return
    }
    func trampoline() {
         for !ctl.Finish {
             eval(ctl)
         }
    }

eval函数需要递归调的地方，改成设置好标记并return，把eval丢一个循环里面，然后判断标记了做相应的动作。由于eval在循环里面是有机会返回的，所以栈不会一直涨，如此实现尾递归。

## 异常机制

其实partial和尾递归都还好，异常机制麻烦一点。kl里面有两个异常处理相关的东西：

    (simple-error str)
    (trap-error (exp xxx) (lambda x (handle x)))

前者会生成一个error对象，而trap-error则是一个特殊表，它先执行里面的表达式(exp xxx)，如果成功就直接返回，如果是error，由将error传给后面的exception handler去处理。

在异常处理这一点上，我觉得kl做法是对的。对比scheme，曾经觉得continuation高大上。现在仔细想想，虽然用continuation是可以实现所有异常机制，但是continuation太难实现了。异常处理属于corner case，所以比较麻烦，adhoc的东西麻烦一点也就那样了，而如果为了解决一个比较麻烦的东西，去实现更麻烦的continuation，这不是缘木求鱼么？

解释器里面的异常处理并不复杂，在前面有了尾递归的基础之后，做异常只需要改一个地方：

    func eval(ctl Control) {
         ...
         ctl.Finish = exception
         return
    }

标记一下异常，然后直接到trampoline去处理。

---------------------------

其实是整个上个周末花了二三天的时间撸的一个东西，结果有几个小坑，导致今天才勉强调通了。[上代码](https://github.com/tiancaiamao/shen-go/tree/b5c2985d8680355acb34e3de7a45c985389e6e2c)。注意到这个项目名用的是`shen-go`而不是kl，其实还是有一点野心的。先写一个解释器熟习一下语言规范，后面考虑做字节码编译器，以及整合Go丰富的标准库。

想体验一下的同学可以这么玩：

    go get github.com/tiancaiamao/shen-go/cmd/kl
    cd $GOPATH/src/github.com/tiancaiamao/shen-go
    $GOPATH/bin/kl -s shen.kl

    Shen, copyright (C) 2010-2015 Mark Tarver
    www.shenlanguage.org, Shen 20.1
    running under Go, implementation: gc
    port 0.0.1 ported by Arthur Mao

    (0.000000-) (define fact
            0 -> 1
            N -> (* N (fact (- N 1))))
    fact

    (1.000000-) (fact 5)
    120.000000

更多细节就参见[Shen的官网](http://www.shenlanguage.org/learn-shen/)了。