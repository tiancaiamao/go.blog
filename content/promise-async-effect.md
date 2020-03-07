## 欢迎来到 javascript 的世界

### callback

callback 不需要语言层面做支持，是一种写法。

callback 其实是手动做 cps，并且是部分 cps。cps 这东西本来就是不好读，不好写，反人类的。

如果把 callback 存储，就是一种捕获 continuation。不过纯粹的 callback 写法都不有把 callback 捕获，都是丢到 event loop 那边去回调的。由于 callback 不是一个 first class 的东西，不好传来传去，不好存储下来，也不好组合。写出来一层一层代码就 callback hell。

### promise

promise 是一个承诺，它承诺到将来某个时候给调用者一个返回值。

假设我们要用这个值了，但是这个值现在还不可用。那以怎么搞呢？我们继续返回另一个 promise。

这个 promise 是说，现在 promise 的值还没好，等 promise 的值好了，我会给你的...

怎么给呢？我们传一个回调...把这个回调先记录下来，等值好了就调用它。

所以对 promise 的访问，不是直接问 value() 好没好，而是提供一个访问函数：

```
then(p promise, cb callback)
```

这个函数的实现大致上是这样子：

```
func then(p promise, cb callback) {
    if p.value != nil {
        // 如果值是 ready 的就直接调用
        cb(p.value)
    } else {
        // 否则呢，要生成另一个 promise
        // 而且 promise 的值好了的时候会调用 callback
        return promise {
            callback = append(callback, cb)
        }
    }
}
```

#### promise 把 callback 变成了 first class 对象

promise 存储 callback 的操作，把 callback 变成 first class 对象。所以在某种意义上讲，promise 可以算捕获了 continuation。

捕获连续我们叫 `call/cc`，那么 promise 其实叫 callcc-and-run-value-with-cc-in-eventloop。那 cc 是啥呢？ cc 其实就是传给 then 的那个访问函数啦。
这个 callback 是当前的未完成的计算，等到拿到了 promise 里面的 value，就可以继续执行了。

#### promise 是一种 monad

promise 用 .then() 串起来的写法，是可以理解成 monad 的

```
promise.then(x, func(x) {do with x})
       .then(y, func(y) {do with y})
       .then()
```

`then` 函数签名是:

```
Promise A -> (A -> Promise B) -> Promise B
```

这是一个典型的 monad 的结构。如果有 haskell 或者 ml 那边的 `|>` 运算符，前面的 then 等价的写法就类似这样子了：

```
x 
|> f
|> g 
|> h
```

promise 的问题是什么呢？ 为什么在函数式语言那边没怎么提这个概念，只在 javascript 里面提。可能是跟 callback 和 event loop 概念比较紧密，而这些是 javascript 世界的东西而不是语言层面的东西。

### generator

generator 概念在 javascript 和 python 里面都有。它可以返回多次，每次调 next 从 yield 的地方继续。

```
function* gen() {
    for (i=0; i<100; i++) {
        yield i
    }
}

g = gen()
g.next()
g.next()
```

yield 这个操作太强大了，它从当前执行状态切换出去，将来还能用 next 函数切换回来。其实也算是 continuation 的概念了。它是一个 delimited continuation，而 generator 函数的入口就是它的界。
这个 continuation 里面要把局部变量之类的状态全部保存下来，这样子将来才能够恢复。

#### generator 是可以编译成一个状态机实现的

 generator 不需要在语言层面特殊支持，理论上只要有闭包的语言都可以实现 generator 。 generator 是可以编译成状态机的：

```
function gen() {
    state = 0
    next =  function() {
        switch (state) {
            case 0:
                // initialize
                i = 0
                state = 1
            case 1:
                // iterator
                if (i < 100) {
                    i++
                } else {
                    state = 2
                }
            case 2:
                // exit
        }
    }
    return next
}
```


emacs 里面的 generator 就是用[库实现](https://github.com/dcolascione/elisp-generators/blob/master/generator.el)的，里面是做了一个 cps 变换。

#### 用 generator 可以实现 coroutine

由于可以 yield，generator 还是很强大的。比较说让多个 generator 之间 yield，也有点 coroutine 的雏形了。


轮子哥的[考不上三本也能给自己心爱的语言加上 Coroutine](https://zhuanlan.zhihu.com/p/25964339) 系列里面，也有讲到实现 coroutine 相关的。

### async/await

promise 还是不够好用，所以后来 javascript 又引入了 async 和 await 这样的语法。

实现层面，可以用不同的方式。也是可以不依赖语言特性实现出来的。比如[这个](https://nullprogram.com/blog/2019/03/10/)。

#### await/async 是可以用 promise + generator 实现的

前面说了，promise 是把 callback 变成 first class 的，而这个 callback 实际上就是 continuation。

generator 在 yield 之后还可以用 next 再次进入，所以实际上也是一种 continuation。如果让两种 continuation 相互跳，就可以用来实现 async 了。

首先观察一下形式的相近：

```
async function f() {
    await url_retrive()
}
```

```
function* gen() {
    yield url_retrive()
    ...
}
```

然后按照语义，await 的是一个 promise，所以我们让 yield 的也是一个 promise。含义是这个 generator 即将要阻塞了，所以放弃当前的计算。
等 promise 好了之后再去调用。用 yield 切出去倒容易，那怎么保证 `url_retrive` 之后的东西是继续跑的呢？ 也就是，yield 放弃掉当前的连续之后，怎么回来的问题。
这里有一个很有技巧的处理：可以让 `url_retrive` 拿到 value 之后，执行 `runGenerator` 函数，执行的 generator 就是它自己！


`runGenerator` 驱动 generator 的不停跑：

```
function runGenerator(gen) {
    for gen.next() {
    }
}
```

yield 的是一个 promise，这个 promise 在 url_retrive 获取到值之后，继续当前的 generator ：

```
function f() {
      // await something equals to yield promise
      yield promise.New(
          url_retrive(url, callback function(res) {
              // the promise use runGenerator to continue f()
              runGenerator(f);
          )
      })
      // remains code
}
```

#### await/async 应该是可以仅用 promise 实现的

promise 的表达能力是强于 await async 的，所有 await async 能实现的功能，用 promise 都能现。反之则不然，那么说明一个表达能力强于另一个。

await 相当于 promise 的的一种受限形式：它一定是等跑完 await 里面的东西返回了，再执行后面的代码的。使用 promise，我们可以做到类似同时等多个操作，这个用 await 做不到。

```
// p1 p2 可以同时做
p1 = promise.New(f)
p2 = promise.New()
...

// p1 完了才搞 p2
p1 = await f();
p2 = g();
```

第一部分讲完了，其实我不是写 javascript 的。只是去了解了一下相关的东西。写 javascript 的人早把这些概念搞得透透的，以至于有点奇技淫巧的味道了。写这部分我是有点贻笑大方，反正网上搜一搜相关的文章是汗牛充栋。Anyway，javascript 找到了一条适合它的方式表达异步，也算能被广大程序员接受的方式，毕竟不是每个程序员的智力都适合学习函数式语言的。

## 函数式编程语言

### cps

cps 是一种连续风格的写法，跟 callback 一样反人类。比 callback 强大一点的是，它把 continuation 暴露出去了。

continuation 是强大的控制流，它代表的是“剩下的计算”。有些语言是支持捕获 continuation 的，比如 scheme 里面的 `call/cc`。在 `call/cc` 面前所有的控制流都是渣渣。它可以实现一切的控制流，比较 try-catch 异常，coroutine，generator 等等。

continuation 的问题是什么呢？这个概念太复杂了，正常人很难理解，也很难用。

cps 变换使得代码变慢，因为中太多的中间代码和闭包生成，性能不太好。所以尽管很强大，但并不实用，一般也只有编译器或者部分的库会使用 cps。

### shift/reset

`call/cc` 弄出来的 continuation，是一种无限制的连续。可以保存下来，再到任何时间，跳转回到这个 continuation。所以这种情况下，完整的上下文都是要保存的。
这也类似有栈协程和无栈协程，需要保存的东西不一样，开销也就不一样。

学术界研究着怎么样减少 continuation 开销，于是搞了 [delimited continuation](delimited-continuation)。常用的接口是 `shift/reset`

`shift/reset` 是弱化版本 `call/cc`。比如说 `<3 + 5 * 2 - 1>` 我们用 `<>` 来表示“界”，然后用 `[.]` 表示连续，`<3 + [.] - 1>` 的含义就是，收到一个值以后，会继续完成剩下的计算，也就是加上 3 和 减去 1。

写成代码：

```
(reset (- (+ 3 (shift (lambda (k) (* 5 2)) 1))))
```

用 reset 来固定“界”，然后用 shift 来捕获连续。

`shift/reset` 的实现方式之一，是做 cps 变换。但是正因为支持 continuation 开销很大才 `shift/reset`，所以全量 cps 显然是很不合理的。

另一种方式是，selective cps，只对部分的代码实现 cps 变换，而大量的代码还是保持默认风格。这样即实现了 delimited-continuation 又保留了性能优势。
还有一种实现，就是魔改 runtime 层了。除了学术圈的东西，好像也很少看到有哪个流行语言里面真的做这个的。

### algebraic effect

接下来要说，是 algebraic effect。这个东西在 haskell 阵营很火。原因是它们要求纯函数式语言是不能有副作用的，而且对类型很强调。

他们搞了 handle 和 effect。effect 这个东西，可以用来描述副作用，并且类型也可以用 row polymorphic 搞。haskell 那一派的人太强调 type 了。

说人话，在我看来就是 try catch 的一个升级版。

```
handle {
    print(x) + 42
} with {
    resume 666
}
```

跟 try catch 的区别是，在 try 里面执行一个语句块，然后 throw 出去，到了 catch 的时候，try 的执行的上下文就扔掉了。

在 handle effect 里面，相当于在 try 里面任何时候可以 throw 出去，而且牛B 的是，catch 里面做一些计算之后，还可以 resume 回来！

algebraic effect 的表达能力是跟 shift reset 等价的。大家都可以用来实现 exception，generator 之类的东西，而且实现方式上面，algebraic effect 跟 `shift/reset` 一样也是可以用 selective cps 实现的。
不同点是 algebraic effect 的表达方式在我看来要人性化一些，就好比 goto 和 if switch while 的区别。毕竟 continuation 的概念真的很难理解，而有 try-catch 的基础的人，你跟他说 effect 就是异常之后还可以从抛出异常的地方 resume 的，这就比较容易理解。

非常推巨硬的这一篇 [paper](https://www.microsoft.com/en-us/research/wp-content/uploads/2017/06/algeff-in-c-tr.pdf)，它是讲用 C 语言实现 algebraic effect 的。

其实就是用 setjump 和 longjump，保存栈，再返回。resume 的实现有一些技巧，需要拷贝栈，再到后面执行恢复。这就跟有栈协程里面 yield 出去保存的操作非常类似了。

关于这一部分，我反对搞 cps ，太蛋疼了，delimited continuation 也反对。至于 algebraic effect 有一定的吸引力。好处是如果在语言层面提供这一个概念，则上层的概念像 exception，coroutine 之类的，都可以基于它实现。如果一项一项的单独实现每一种机制，复杂度就很分裂了。比如说，在实现 generator 里面要考虑语言支持了异常的，特性的不是加法，复杂度是指数叠加的。函数式语言的阵营还是太小众了，这边的很多概念都不大众熟知，其主要原因还是因为学习门槛太高了，对小白不友好。

## 实用主义致上

### coroutine

大家知道图灵和丘奇，一派是搞机器，一派是 lambda。实用主义致上的，目前都是搞机器的。coroutine 这是个好东西。搞机器的这派人，典型的就像 Go 的 goroutine 的实现方式。

C 这边虽然有一些库，但是都没能将 coroutine 发扬光大。语言层面缺少闭包还是不太好用。

### fiber

fiber 这个跟 coroutine 差不多是一个东西。按照某种说法是，coroutine 是不带调度的，从一个 coroutine 切换到另外一个 coroutine。fiber 是要带调度的，yield 不是直接切换到另一个 fiber 而是走到调度，再用调度决定切换到哪边。我觉得有一点扣字眼，总体上感觉都差不多。

### try catch

很多语言都提供了 try catch，这个算是更普通的吧。java 不用说，像 Go 里面的 defer 其实也是一种异常机制。

甚至在 C 语言里面，setjmp longjmp 也是 try catch，只不过不是做成语言层面的。
