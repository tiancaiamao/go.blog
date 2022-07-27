整个周末这两天都在密集地写代码，为 [cora 语言](cora.md)实现编译，以至于家里人认为我有点走火入魔了。是啊，掐指一算，又一个七夕都快要到了，为毛我今年还在敲代码？[去年的这个时候](chinese-valentine-day.md)就挺魔性的。进入正题，今天说的是 cora 语言的编译器的实现方案。

之前我在实现 shen 语言的时候，快速撸了一个纯 AST 的解释器，然后实现了字节码的编译和虚拟机和[优化](shen-go-v0.2.md)，以及[后来的一些探索](shen-go-next-notes.md)，再就感觉那个方向能优化的东西做不下去了。这次在实现 cora 语言的时候，我直接先回退到了更简单的解释器方案。不过之前已经提到过了，来自 gambit 的启发 -- **语言即虚拟机**。我准备把 cora 编译到其它语言，最终直接生成二进制，像最近出来的 v 语言就是这么干的。

gambit 将 scheme 生成到 gvm 的虚拟机，gvm 虚拟机的设计很奇特，它设计的目的是用于做代码生成的，它可以适配到各个语言上面，默认是生成 C 代码，最新的 gambit 似乎把生成到 javascript 之类的也支持了。

实现里面会先做 cps 变换，经过 cps 变换之后，函数调用就直接可以变成 jump 了。然后是闭包变换，处理 scheme 编译到 C 的时候的闭包。gvm 生成的代码不会使用宿主机语言的栈，也不会使用函数调用协议，它只用到了最基本的一些东西：变量，跳转这些。每个语言至少都有这些，如此一来，gvm 可以非常方便地生成到各种语言。

性能方面，生成出来的代码跟原生的 C 代码是一样性能的。看看 [scheme 语言实现的性能 PK](https://ecraven.github.io/r7rs-benchmarks/)，你会发现 gambit 是排在 top3 的。

gambit 的作者做过一个 [scheme 编译到 C](http://churchturing.org/y/90-min-scc.pdf) 的 talk，网上搜索 "90 minute Scheme to C compiler" 能找到视频。在那个 demo 里面，代码生成是生成到了一个基于栈的虚拟机。这是一个简化的版本，真实的 gvm 设定是有寄存器的设计的。

我觉得在代码生成这种场景下，设计成栈虚拟机还是挺影响性能的。所以在 cora 的实现里面，我做成了一个基于寄存器的虚拟机。具体的 cps 变换和闭包变量跟那个 "scheme 编译到 C" 里面讲的做法差不多，其实我[以前也做过类似的东西](scheme-to-c.md)，只是每回头看一遍，都会多加深一些理解。我们来重点看一下代码生成的过程。

如果是基于栈虚拟机，编译的时候 `(+ a b)` 就是编译 a，进栈，编译 b，进栈，然后执行加操作。代码生成类似于：

```
push(stack, a)
push(stack, b)
add
```

在 cora 里面，我是拿语言的局部变量当寄存器用了，这样一来，语言就是一个无限寄存器虚拟机。为每一个表达式，结果都将会对应分配到一个寄存器(变量)上。


```
reg1 <- a
reg2 <- b
reg3 <- (+ reg1 reg2)
```

虚拟机的定义类似这样子：

```
type VM struct {
	pc    func(*VM1)
	stack []Obj
}
```

这个代码生成的算法实现是，维护一个变量到"寄存器"的映射 m，如果遇到新变量，则把映射添加到 m 里面。如果不是新变量，就直接返回寄存器值。寄存器数量是无限的，每次需要新的 reg 加加就好了。

函数调用协议是走 `VM` 里面的 `stack` 进行数据交换，不使用宿主语言原始的栈。这样参数是从虚拟机的栈里面拿到，比如这个函数头部：

```
(lambda (a b c) ...)
```

生成得到：

```
reg1 <- vm.stack[0]
reg2 <- vm.stack[1]
reg3 <- vm.stack[2]
...
```

同时修改变量到寄存器的映射：

```
a => reg1
b => reg2
c => reg3
```

如果后面代码中再遇到，比如 `(+ a b)` 的时候，由于 a b 都分别分配在 reg1 reg2 上面，所以就是

```
reg3 <- (+ reg1 reg2)
```

对于 if 我暂时没有弄成跳转，而是使用语言本身的 if 指令。 `(if a 1 b)` 这个生成出来是类似于：

```
reg1 <- a
reg2 <- 1
reg3 <- b
if reg1 { reg4 <- reg2 } else { reg4 <- reg3 }
```

函数调用的生成，就是直接 jump，因为经过 cps 变换是不需要再返回的。假设变量 f a b c 在映射中对应的寄存器分别是 reg0 reg1 reg2 reg3，函数调用的代码：

```
(f a b c)
```

将会生成

```
vm.pc <- reg0
vm.stack[0] <- reg1
vm.stack[1] <- reg2
vm.stack[2] <- reg3
jump
```

如果换一个角度看，其实生成结果是一个 [ssa](https://en.wikipedia.org/wiki/Static_single_assignment_form) 形式的中间表示！所以如果想生成到比如 LLVM 的 IR，理论上也是可行的。不过生成到语言的好处，是不用拘泥于形式，语言即虚拟机。像使用 LLVM 的话这里的函数调用协议就受到限制了。反正最终都变成了机器指令，而且性能上也不会有啥差别。

开发和调试的时候，用解释器就够用了。只有真正上线生产使用才需要编译。目前编译到 Go 代码，就是用 cora 的解释器写的，完全不用考虑性能。


看一下最终效果，先是 cps 变换：

```lisp
198 #> (cps-convert '(do
                  (set 'square (lambda (x) (* x x)))
                  (+ (square 5) 1)))
((lambda (#arg39291) ((lambda (_) ((lambda (#f39238) (#f39238 (lambda (#arg39211) (halt (+ #arg39211 1))) 5)) square)) (set (quote square) #arg39291))) (lambda (#k39311 x) (#k39311 (* x x))))

``` 

或许写成 let 形式好读一些， `((lambda (a b) body) 1 2)` 跟 `(let ((a 1) (b 2)) body)` 等价的，简化一下就是：

```
(let #arg36517 (lambda (#k36537 x)
                 (#k36537 (* x x)))
     (let _ (set (quote square) #arg36517)
          (square (lambda (#arg36437)
                    (halt (+ #arg36437 1))) 5)))
```

然后做闭包变换：

```lisp
197 #> (closure-convert '((lambda (#arg36517)
   ((lambda (_)
      ((lambda (#f36464)
         (#f36464 (lambda (#arg36437)
                    (halt (+ #arg36437 1))) 5)) square))
    (set (quote square) #arg36517)))
 (lambda (#k36537 x)
   (#k36537 (* x x)))))
((#clofun39179 (lambda () ((lambda (#arg36517) ((lambda (_) ((lambda (#f36464) ((%closure-func #f36464) #f36464 (%closure #clofun39075) 5)) square)) (set (quote square) #arg36517))) (%closure #clofun39175)))) (#clofun39175 (lambda (#clo38831 #k36537 x) ((%closure-func #k36537) #k36537 (* x x)))) (#clofun39075 (lambda (#clo38586 #arg36437) (halt (+ #arg36437 1)))))
198 #> 
```

写成简化后的 let 是：

```
((#clofun37868
  (lambda ()
    (let #arg36517 (%closure #clofun37864)
         (let _ (set (quote square) #arg36517)
              ((%closure-func square #f36464)
               #f36464 (%closure #clofun37764) 5) ))))
 (#clofun37864
  (lambda (#clo37520 #k36537 x)
    ((%closure-func #k36537) #k36537 (* x x))))
 (#clofun37764
  (lambda (#clo37275 #arg36437)
    (halt (+ #arg36437 1)))))
```

这里变成了三个全局函数，lambda 变成了 `%closure`，函数调用变成了 `(%closure-func ...)`。

再经过 code-generate 之后，变成这样的三段代码：

```
199 #> (code-generate '((#clofun37868
  (lambda ()
    ((lambda (#arg36517)
       ((lambda (_)
          ((lambda (#f36464)
             ((%closure-func #f36464)
              #f36464 (%closure #clofun37764) 5)) square))
        (set (quote square) #arg36517)))
     (%closure #clofun37864))))
 (#clofun37864
  (lambda (#clo37520 #k36537 x)
    ((%closure-func #k36537) #k36537 (* x x))))
 (#clofun37764
  (lambda (#clo37275 #arg36437)
    (halt (+ #arg36437 1))))))
(((label #clofun37868) (<- #reg39402 closure #clofun37864) (<- #reg39465 intern square) (<- #reg39488 (set #reg39465 #reg39402)) (<- #reg39553 square) (<- pc #reg39553) (<- #reg39669 closure #clofun37764) (<- #reg39689 5) (<- stack 0 #reg39553) (<- stack 1 #reg39669) (<- stack 2 #reg39689) jump) ((label #clofun37864) (<- #reg39725 stack 0) (<- #reg39729 stack 1) (<- #reg39733 stack 2) (<- pc #reg39729) (<- #reg39894 (* #reg39733 #reg39733)) (<- stack 0 #reg39729) (<- stack 1 #reg39894) jump) ((label #clofun37764) (<- #reg39927 stack 0) (<- #reg39931 stack 1) (<- #reg40005 1) (<- #reg40006 (+ #reg39931 #reg40005)) (<- stack 0 #reg40006) halt))
```

其实是三个函数：

```
(label #clofun35259)
(<- #reg35801 closure #clofun35255)
(<- #reg35864 intern square)
(<- #reg35887 (set #reg35864 #reg35801))
(<- #reg35952 square)
(<- pc #reg35952)
(<- #reg36068 closure #clofun35155)
(<- #reg36088 5)
(<- stack 0 #reg35952)
(<- stack 1 #reg36068)
(<- stack 2 #reg36088)
jump
```



```
(label #clofun35255)
(<- #reg36124 stack 0)
(<- #reg36128 stack 1)
(<- #reg36132 stack 2)
(<- pc #reg36128)
(<- #reg36293 (* #reg36132 #reg36132))
(<- stack 0 #reg36128)
(<- stack 1 #reg36293)
jump
```

```
(label #clofun35155)
(<- #reg36326 stack 0)
(<- #reg36330 stack 1)
(<- #reg36404 1)
(<- #reg36405 (+ #reg36330 #reg36404))
(<- stack 0 #reg36405)
halt
```

只做到了生成出虚拟的指令，还没有做完自动转 Go 的那一步。
如果手动转换成 Go 代码，效果是这样子的：

```
func clofun35259(m *VM1) {
	reg35801 := makeClosure(clofun35255)
	reg35864 := MakeSymbol("square")
	reg35887 := funSet(reg35864, reg35801)
	reg35952 := reg35887
	m.pc = closureFn(reg35952)
	reg36068 := makeClosure(clofun35155)
	reg36088 := makeInteger(5)
	m.stack[0] = reg35952
	m.stack[1] = reg36068
	m.stack[2] = reg36088
	return
}

func clofun35255(m *VM1) {
	// reg36124 := m.stack[0]
	reg36128 := m.stack[1]
	reg36132 := m.stack[2]
	m.pc = closureFn(reg36128)
	reg36293 := primNumberMultiply(reg36132, reg36132)
	m.stack[0] = reg36128
	m.stack[1] = reg36293
	return
}

func clofun35155(m *VM1) {
	// reg36326 := m.stack[0]
	reg36330 := m.stack[1]
	reg36404 := MakeInteger(1)
	reg36405 := PrimNumberAdd(reg36330, reg36404)
	m.stack[0] = reg36405
	m.pc = nil
	return
}
```


这里是可以运行完整的[生成后的代码](https://github.com/tiancaiamao/cora/blob/fe413b408656d4e88c6d259f77484d802c5addbf/kl/aaa_test.go)。 例子的输入是用的 `(+ (square 5) 1)`， 执行最终结果会得到 26，嗯，验证想法没啥问题。

撸了一个周末，总算是一点阶段性的成果吧，[上代码](https://github.com/tiancaiamao/cora/blob/532c18c8d43bc9fcf6fdf139c5df55c91951cc62/compiler.cora)。
