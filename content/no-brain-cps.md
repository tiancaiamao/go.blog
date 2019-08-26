CPS 变换真是一个让人又爱又恨的技术，在 PL 学术界也是引发无数讨论的话题。按我对 scheme 语言这么多年的接触，也是经历肯定，否定，再肯定，再否定...无休止的过程，不过每次理解都会更深一点，倒是好的。老实说，是好是坏，还是看它带来多少好处，以及有多少缺点。比如，反方观点：

* CPS 过后的代码，简直没法读。
* CPS 变换引入了一大堆多余的代码，后期又需要额外添加步骤，将代码给优化掉，凭空增加了复杂性。
* CPS 之后的代码永不返回，直接转成 C 会爆栈。
* CPS 引入了很多闭包，会影响性能。

正方观点：

* CPS 之后得到的 IR 更容易实现一些优化过程
* CPS 完全将控制流暴露出来，使 call/cc 功能非常容易实现
* CPS 之后调用变成变成带参数的跳转，简化了函数调用协议

先看一下如何实现 CPS 变换。网上能找到不少教程，包括王垠的 40 行代码。不过，那段代码太精妙了，以至于我没法知道它背后的推导过程（推导过程才是对理解至关重要的）。所以还是从最基础的方式开始，大致的变换规则如下：

```lisp
(func cps
      lit cc => [cc lit] where (or (variable? lit) (const? lit))
      ['if a b c] cc => (let va (gensym 'r)
                          (cps a ['lambda [va]
                                   ['if va
                                        (cps b cc)
                                        (cps c cc)]]))
      ['lambda args body] cc => (let k (gensym 'k)
                                  [cc ['lambda (cons k args)
                                        (cps body k)]])
      [f x] cc => (let f0 (gensym 'f)
                    (cps f ['lambda [f0]
                             (cps x ['lambda [x]
                                      [f0 cc x]])])))
```

这样子变换出来的结果问题比较多，比如说我们变换这段代码：

```
(do (set 'square (lambda (n) (* n n)))
    (+ (square 3) 1))
```

会得到这样的结果：

```
((lambda (#arg4273)
   ((lambda (_)
      ((lambda (#f4236)
         (#f4236 (lambda (#arg4217)
                   ((lambda (x) (halt x)) (+ #arg4217 1))) 3))
       square))
    (set (quote square) #arg4273)))
 (lambda (#k4287 n)
   (#k4287 (* n n))))
```

这个可读性真够差的，我们可以手动简化。首先是不必要的 lambda，如果一个 lambda 是这种形式：

```
((lambda (x) body) v)
```

我们都可以改写成 let：

```
(let x v body)
```

上面的代码简化得到：

```
(let #arg4273 (lambda (#k4287 n)
                (#k4287 (* n n)))
     (let _ (set (quote square) #arg4273)
       (let #f4236 square
            (#f4236 (lambda (#arg4217)
                      (let x (+ #arg4217 1)
                        (halt x))) 3))))
```

其实改成 let 形式以后，并没有引入太多额外的 lambda。

另外我发现，`let #f4236 square` 这个改名跟本没啥意义。于是我们可以引入一个规则：

如果函数调用的参数本身是一个符号，并不需要对它这样改写：

```
      [f x] cc => (let f0 (gensym 'f)
                    (cps f ['lambda [f0]
                             (cps x ['lambda [x]
                                      [f0 cc x]])]))
```

可以直接用 f 就行了：

```
      [f x] cc => (cps x ['lambda [x0]
                            [f cc x0]])
```

同理，对参数也可以这样改写，省掉不必要引入的变量，也就是说如果 `x` 已经是最简形式，其实等价于：

```lisp
      [f x] cc => [f cc x]
```

另外，如果一个参数只出在 lambda 的 body 中出现一次，我们可以用实参替换掉 body 内的形参，并不会导致结果膨胀：

```lisp
(let x (+ #arg4217 1)
                   (halt x))
```

直接变成

```lisp
(halt (+ #arg4217 1))
```

最终上面的例子变成，还算能读：

```lisp
(let _ (set (quote square)
            (lambda (#k4287 n)
              (#k4287 (* n n))))
  (square (lambda (#arg4217)
            (halt (+ #arg4217 1))) 3))
```

这些都是我自己在手动改写简化代码过程中，总结的一些规律。后来偶然读到一篇 paper，居然跟这个做法不谋而合。这篇 paper 叫 《No-Brainer CPS Conversion》(所以博客题目也叫无脑变换)，它先没有追求所谓的 one-pass，在最原始的 CPS 变换之后，引入了一个"无脑"优化阶段，使用的规则如下：

### 下面这种形式，如果参数 y 是简单形式(variable,const 这类)，则可以执行 beta-替换

```
((lambda (x) .. x .. x .. x) y) => .. y .. y .. y
```

### 如果参数 x 在 body 里面只有一个位置引用，则可以执行 beta-替换

```
((lambda (x) .. x ..) triv) => .. triv ..
```

### 如果参数在 body 里面根本没使用，则可以返回 body

```
((lambda (x) body) triv) => body
```

### eta 变换

```
(lambda (x k) (triv x k)) => triv
```

经过“无脑”变换之后，得到的结果还算是比较简洁的。比如 fact 的例子：


```lisp
(do (set 'fact (lambda (n)
                 (if (= n 0)
                     1
                     (* n (fact (- n 1))))))
    (fact 5))
```
 
 比较 naive 的变换：
 
 ```lisp
((lambda (#arg4364)
   ((lambda (_)
      ((lambda (#f4327)
         (#f4327 (lambda (x)
                   (halt x)) 5))
       fact))
    (set (quote fact) #arg4364)))
 (lambda (#k4378 n)
   ((lambda (#r4387)
      (if #r4387 (#k4378 1)
          ((lambda (#f4428)
             ((lambda (#arg4434)
                (#f4428 (lambda (#arg4414)
                          (#k4378 (* n #arg4414))) #arg4434))
              (- n 1)))
           fact))) (= n 0))))
 ```
 
 经过无脑优化之后：
 
 ```lisp
(let _ (set (quote fact)
            (lambda (#k4874 n)
              (if (= n 0)
                  (#k4874 1)
                  (fact (lambda (#arg4919)
                          (#k4874 (* n #arg4919)))
                        (- n 1)))))
  (fact (lambda (x)
          (halt x)) 5))
 ```

好啦，做到这里的时候，我发现一个严重问题！CPS 变换引入的有部分 lambda 是怎么都消除不掉的！比如这里的传给 fact 的 cc，它是

```lisp
(lambda (#arg4919)
        (#k4874 (* n #arg4919)))
```

这个会有 free variable，经过闭包变换之后，会导致有创建闭包的开销。

我发现有些情况变换出来有，有些情况变换出来不会引入额外的闭包。总结出来，非尾调用会引入额外的闭包。

原来是这样子的，由于 CPS 不带返回了，原本的栈桢就消失了。但是对于非尾调用，函数总要返回到当时的上下文里面呀，没有栈了怎么办？其实，经过 CPS 变换是把原本栈的东西，拍成 active frame 了，也就是原来可以用栈存的东西，现在都跑到 cont 里面去了，变成了 free variable！只要支持任意跳转(也就是 call/cc)，这个是不可能优化掉的。

后面发现有些 paper 优化这些东西，比如 《Callee-save registers in continuation-passing style》，在我看来，这个事情本质上解法都很丑陋。也就是**经过 CPS 变换，在非尾调用的场景，原来可以放到栈里面的东西，现在都要变成 cont 里面的自由变量，导致闭包创建**。这个开销本质上不可以消除。即使想优化这里也很绕。

所以我的结论是，还是不做 CPS 了。

再扯几句无关的，理论上我在寻求一个比较好的 cora 编译到 C 的方案，中间会引入一个虚拟机设计，原本是做了 CPS 变换的。但是想想这一步，不一定值得。cora 需要支持的 feature 包括：

* 闭包
* TCO
* Curry

而 call/cc 和 exception，暂时还不属于必须的 feature。如果用 CPS 实现，可以获得 TCO，call/cc，exception，但是后两者并非必须，所以我在重新考虑这个是否值得。这个取舍过程就要评估 CPS 本身的优缺点了。

这个评估过程中，首先看看能将 CPS 优化到何种程度，然后找到了一种无脑优化方式，还算可接受。再然后，又发现有些几乎完全无法优化掉的开销引入，于是就变得厌恶了。

如果不做 CPS，TCO 该怎么实现呢？对于尾调用可以用 trampoline，对于非尾调用直接用 C 语言的函数调用协议应该就行了。
