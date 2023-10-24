guile 在做 wasm 支持，搞了一个叫 [hoot](https://spritely.institute/hoot/) 的项目，这是一个 scheme to wasm 的项目，项目牵头的是 Andy Wingo,也就是 gnu guile (scheme) 的作者。Wingo 最近这一二年心思似乎都是用在 wasm 上面。我读到他博客里面[这篇](https://wingolog.org/archives/2023/03/20/a-world-to-win-webassembly-for-the-rest-of-us)，里面写了 scheme to wasm 中会遇到的一些挑战，然后就知道了 tailification 这个好玩的东西。

(再多说一句，scheme 是学院派的，而 guile 这种是偏工业界的，学院派很多落实到项目上面的不靠谱，而率先让 scheme 落地到 wasm 很可能就是 hoot 了，我比较看好这个项目)

scheme 编译到不管是 wasm 或者是 C，里面都有一个不太好解决的地方是 continuation。continuation 是 scheme 语言里面有的，而大部分其它语言都没有的东西。

实现 delimited continuation 有好多条路子，大抵可以分为两派：机器派和语言派，其它的都可以算作两派的混合或者折中实现。

- 纯的机器派实现方式，典型的就比如用 C 实现然后通过 setjump/longjump 拷贝和恢复栈，像云风的 coroutine 库那种。包括 [koka 语言](koka-papers.md)早期用的 libhandle 方式也是。
- 纯的语言派实现方式，就是做 CPS 变换，像 chicken scheme 那种就是的。
- 机器派和语言派的中间方案就是虚拟机，像 SECD 虚拟机里面就是 env / continuation 都在虚拟机里面的。还有我之间介绍过 [gambit scheme](gambit-callcc.md) 中的实现方式也是用的虚拟机。

机器派的问题是太多低层了，C语言限制比较多，有些甚至得用汇编搞。而如果语言派用的 CPS 搞法，要么得做非常大量的激进优化，要么性能就不行。所以 gambit scheme 引入的虚拟机方式其实是在一个比较好的平衡上面。在此之前我并没有注意 guile 的实现方式...然后发现它们用的 tailification 的做法之后，觉得也是条不错的路子。


在我们看 tailification 之前，先看看 CPS 的方式。

```
(define (f x y)
  (+ x (g y))
```

通过 cps 之后就变成了

```
(define (f x y cc)
	(g y (lambda (v)
	       (cc (+ x v)))))
```

所有函数调用都变成了尾调用，不再返回。那么对于非尾调用的函数进行 cps 变换之后，局部变量去哪儿了呢? 比如说这里的 x，它就成了闭包变量!
也就是说，只要函数调用返回之后，还会被用到的变量，最终都变成了闭包变量。做 cps 变换的结果，会导致生成大量的闭包。生成闭包对象是要导致内存分配的，所以就导致性能不行。

(额外说一个，所谓的无栈协程vs有栈协程，无栈是没有栈的，那么局部变量的值存储在哪儿呢? 都类似于这里的 cps，变成闭包变量，存储在堆上去了...性能自然也就"呵呵"了)


假设我们通过 VM 方式实现，函数传参用栈，则 continuation 可用直接拷贝 VM 的栈的方式获取。让我们看看这个过程，上面的代码中变量 x 是如何处理的。

```
local-ref 1  ;; 获取 x
push         ;; x 进栈

global-ref g  ;; 准备调用 (g y)
local-ref 2   ;; y 进栈，传递第一个参数
call

prim-add      ;; 执行 '+'操作
```

在进入函数 g 里面之后，参数是通过栈传递过来的，这个时候 x 用 local-ref 1 获取到

```
local-ref 1  ;; 函数 g 的代码，如果需要获取 x
```

VM 有自己的栈，局部变量在 VM 的栈里面，不会像 CPS 变换那边导致大量的闭包创建开销，并且暴露的 VM stack 可以用于实现 continuation。

这个方式不好的一点在于，我们定义的一套栈虚拟机，并且函数传参也是走的栈传参，如果还是有一定性能开销的。如果想优化，就要像 gambit scheme 那样定制寄存器虚拟机，它还在这一层做了寄存器分配！

**有没有一种可能是，尽量地重用 C 语言的基础设施，比如说，局部变量尽量就用 C 语言的栈/寄存器，而不是用 VM 的栈，同时又能支持 continuation 呢?**
这就是 talification 能够解决的问题。

如果我们仔细观察可以发现，CPS 变换的结果中，只有在非尾调用的函数调用之后，继续被访问的变量，是需要存放的。比如说，还是这个例子:


```
(define (f x y)
  (+ x (g y))
```

x 是函数 f 里面的局部变量，它在 `(g y)` 调用之后，还存活着，所以 cps 变换过后，它需要变成闭包变量。而 y 变量，则在函数 `(g y)` 调用之后，再也不会使用了，于是 y 的生命期到这里就结束了。

Wingo 在 scheme to wasm 里面提到的 tailification 的做法就是，只对像 x 这样的变量，似乎于 VM 的方式维护在 VM 的栈上分配，而像 y 这样的变量则可以在宿主语言的栈上。由于我们自己的 VM 栈是暴露出来的，我们仍然可以通过拷贝自己维护的栈，来实现 continuation。对应的最关键的几页 PPT 是这样的：

Delimited continuations are stack slices

Make stack explicit via minimal continuation-passing-style conversion

- Turn all calls into tail calls
- Allocate return continuations on explicit stack
- Breaks functions into pieces at non-tail calls


Before a non-tail-call:

- Push live-out vars on stacks (one stack per top type)
- Push continuation as funcref
- Tail-call callee

Return from call via pop and tail call:

(return_call_ref (call $pop-return)
                 (i32.const 0))
After return, continuation pops state from stacks

那边的博客里面也给了一个具体的例子：

```
(define (f x y)
  (push! x)
  (push-return! f-return-continuation-0)
  (g y))

(define (f-return-continuation-0 g-of-y)
  (define k (pop-return!))
  (define x (pop! x))
  (k (+ x g-of-y)))
```

至于实现细节，它则是直接给的代码 tailify.scm，我就没有去读了。这儿理论上可以通过静态分析 liveness analyze 实现。

我自己思考了一下，倒是想了一个自己的方式：先做闭包变换，后做 cps，然后再一遍类似闭包变换的操作。


```
(define (f x y)
  (+ x (g y))
```

经过闭包变换之后


```
(set 'f (closure (x y) ()
			(+ x (g y))))
```

然后执行 cps 变换得到


```
(set 'f (closure (x y) ()
	  (call g y (continuation (val)
				  (+ x val)))))
```

可以看到这里面的 x 即不是一个局部变量，又不是一个全局变量，也不是像加减乘除这种 primitive 操作。

```
(continuation (val)
				(+ x val))
```

这时我们可以再使用一遍类似闭包变换的方式，将这一类变换全部找出来，这样的变量就是我们需要在栈上保存的变量。

```
(set 'f (closure (x y) ()
	  (call g y (continuation (val) (x)
				  (+ x val)))))
```

我所使用的表示是这样的形式，第一个列表里面是局部变量，第二个列表则是 freevars or stackvars。


```
(closure (locals...)  (frees...) ...)
(continuation (retval) (stacks...) ...)
```


然后再做代码生成。代码生成时，对于 (call .. (continuation ..)) 这种东西，需要准备好参数和返回的continuation。
这里可以利用一下 gambit scheme 那边的技巧，不用对每个函数都加一个 continuation 参数，而是统一藏在 VM 的结构里面。

```C
void f(struct VM *vm, Obj n) {
	saveCont(vm, f_return_cc); // save pos and push continuation
	push(vm, n);    // push n
	g(vm, y);
}

void f_return_cc(struct VM *vm, Obj val) {
    Obj x = pop(vm);
	primAdd(vm, x, val);
}

void primAdd(struct VM *vm, Obj a, Obj b) {
    cc = popCont(vm);
	cc(a+b);
}
```

**为什么是先做闭包变换，再做 CPS 呢，正常方式应该是先做 CPS，再做闭包变换的呀？这正是处理得比较巧妙的一个点**。如果先 CPS 了，正常的 freevars 跟 CPS 变换后引入的 freevars 就混到一起了，不好区分哪些是必须成为闭包的变量，哪些是可以在 VM 栈上分配的变量。而如果先做闭包变换，则可以很容易知道 CPS 过后的 freevars 都不是真正的 freevars，可以栈上分配。


代码还没写，只是先的想法整理下来...
