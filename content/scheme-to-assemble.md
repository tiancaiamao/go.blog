本文将描述如何一步步将scheme编译成汇编代码。这些编译过程其实对于其它高级语言都是同样适用的。

为了方便描述，这里挑选了scheme的子集，只选择了if begin set! lambda let这几个关键字。条件，循环，分支都是可以用这些关键字实现的。简单起见，基本数据类型也假设只有整数。

汇编有什么？寄存器，赋值，跳转指令。从scheme生成汇编代码，就是将条件、循环、分支消除，将变量变成寄存器。

## 闭包变换

closure-convert将函数提升到全局，消除闭包。

    (let ([a 3])
        (lambda () (+ a 1)))

这里就是一个闭包，a是自由变量。经过闭包变换，lambda被变成了code表，闭包使用closure表生成：

    (program ([f$1 (code (a) ()
                    (+ a 1))])
        (let ([a 3])
            (closure f$1 a)))

一个闭包对象包含了代码，以及自由变量。这里分别是f$1和a。

## 去掉let

remove-let将let表去掉，并引入了locals表。

    (let ([a 3] [b 5])
        (+ a b))

被变化为：

    (locals (a b)
        (set! a 3)
        (set! b 5)
        (+ a b))

## 简化结构

remove-complex-opera将嵌套的操作扁平化。事实上，到目前为止if begin set!以及函数调用都是可能相互嵌套的，一个语言的结构可能十分复杂，比如说：

    (locals (a b c d e f w)
        (set! a (if b (begin c (set! d 6) (f e (+ d 1))) w)))

可以引入一些中间变量，在不改变语义条件下，将代码结构进行简化。

    (locals (a b c d e f w t1 t2 t3)
        (begin
            (if b
                (set! t1
                    (begin c
                            (set! d 6)
                            (set! t2 (+ d 1))
                            (set! t3 (f e t2))
                            t3))
                (set! t1 w))
            (set! a t1)))

看起来还是挺复杂，举个更简单的例子吧：

    (f (+ a b))

会被变成

    (set! t (+ a b))
    (f t)

简化之后，每次都只执行一步的操作。

## 调用协议

impose-calling-convertions执行函数调用协议。具体的，包括准备函数现场，进入函数调用，函数返回。

    (program ([f$1 (code () (a b) (+ a b))])
        (f$1 5 6))
        
准备函数现场，(f$1 5 6)会被改写，只有参数的进栈出栈，以及跳转：

    (set! in0 5)
    (set! in1 6)

in0和in1分别是栈上的位置，用于传递参数。进入函数后会先从它们(栈上位置)获取参数。

    (code ()
        (locals (a b)
            (set! a in0)
            (set! b in1)
            (+ a b)))

## 寄存器分配

assign-registers要将变成分配到对应的寄存器中。

    (code ()
        (locals (a b t)
            (set! a in0)
            (set! b in1)
            (set! t (+ a b)
            (+ t 1))))

假设这里为a b t分配的寄存器分别是rbx rcx rdx，则经过寄存器分配后：

    (code ()
            (set! rbx in0)
            (set! rcx in1)
            (set! rdx (+ rbx rcx)
            (set! rax (+ rdx 1))))

这里是把返回值放到了rax中。

## 去掉if

看似离汇编代码越来越近了。其实由于有if的嵌套的存在，还是可能有复杂的结构的。我们需要remove-if这个过程。它将去掉if，引入等价的跳转语句。

    (program
        ([f$1 (code ()
            (if a (if b c d) e))]))

被改成

    (program
        ([f$1 (code ()
            (if a (jump t$1) e))
         [t$1 (code () (if b c d))]]))

添加了一些跳转的标记，在if里面不会再存在嵌套的结构。

## 扁平化

flatten-program是会将代码拉平。现在的代码中，我们有一个全局的program字段，里面有很多的code段代码。

    (program
            ([f$1 (code ()
                (if a (jump t$1) e))
            [t$1 (code () (if b c d))]]))

扁平化之后，就基本上等价于汇编了：

    (program
        f$1
        (if a (jump t$1))
        e
        t$1
        (if b c d)
    )

## 总结

scheme编译成汇编的过程，就是一步步的把if lambda set以及嵌套结构、变量全都去掉，从而将高级语言转化为汇编语言。让我们回头看一遍：

1. 闭包变换--去掉了lambda
2. 引入局部变量和locals表--去掉了let
3. 简化结构以及调用协议--去掉了函数调用以及复杂的嵌套语句
4. 寄存器分配--去掉了变量
5. 去掉if

本文只不为了说清楚这个过程，把很多细节都简化了。存在赋值时其实不能直接做闭包变换的；string等数据类型也需要更多的处理；scheme很有特点的连续在这里也没有涉及；运行时更是很大的一块。
