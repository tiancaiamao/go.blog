比如，我们需要提供一个随机数的功能:

- (seed) 返回一个种子。
- (rand N) 接受一个种子，返回一个随机数。同时，返回的随机数需要作为新的种子供 rand 下次调用使用

这个 rand 是需要一直维护种子的上下文的，在命令式语言里面，我们可以用全局变量：

```
var s = seed()
func randAPI() {
   s = rand(seed) 
   return seed
}
```

这里的 s 是一个全局变量，对外供的 randAPI 不是一个纯函数，它更新了全局变量 s 是带了副作用的。
在函数式语言里面，我们没办法像这样使用全局变量和赋值来维护 rand 调用的种子上下文。
rand 必须像 monad 那样，接受一个种子，返回一个值和一个新的种子。

```
(defun rand (M f)
    (let val (| (>> (+ M 33421637) 12) 0x52671) ;; 示例瞎写的，通过种子进行位shuffle得到新的随机数
        (f val)
        (rand val)))

(let M (seed)
    (rand M f))
```

rand 返回的值是返回给 f 的，返回的新的种子是供 rand 下一轮调用的。如果正统的 monad 写法应该是让 f 的返回值返回新的种子，也就是 monad M。这里不纠结，让调用者 f 不管返回值，rand 函数自己去调下一轮的 rand。

如果我们一直维护着这个种子上下文，就可以一直这么用:

```
(let M (seed)
    (bind (rand M) (lambda (v)
                    ;; this is function f, it receive rand value v
                    ...
                    (bind (rand v)
                         ...))))
```

```
rand (seed) |>  (lambda (v) ;; f use v and return a new monad
    |> f2
    |> f3
```

只要满足 M 是全局的，并且在调用链过程中一直传递这个 M 就可以。当全局就只有一个 monad 的时候可以这么玩，但是实际代码逻辑复杂之后，就没法这么玩了。不可能把所有的需要全局变量的地方，都提到一个顶层 let 里面，并且让每个函数去处理所有的 monad:

```
(let M1 (monad)
     M2 ..
     M3 ..
     (my-code ..
```


**这里需要用一个技巧，把全局变量做成全局函数的闭包变量，再把全局函数暴露出去**。还是看上面的 rand 的例子：


```
(defun rand (M f)
    (let val (| (>> (+ M 33421637) 12) 0x52671) ;; 示例瞎写的，通过种子进行位shuffle得到新的随机数
        (f val)
        (rand val)))

(let M (seed)
    (defun run-rand (f)
        (rand M f))
    run-rand)
```


rand 是一个纯函数前面已经写了，不用再提。

```
(let M (seed)
    (defun run-rand ()
        ...
```

**我们返回一个全局函数 run-rand，将 seed 做成这个函数的闭包值**。注意这个参数 f:


```
    (defun run-rand (f)
        (rand M f))
```

我们的使用方式不再是让 run-rand 返回 rand 值

```
v1 = run-rand() 
v2 = run-rand() 
```

而是像 cps 写法那样传递访问函数 f 进去

```
(run-rand (lambda (v)
    ... ;; do anything with the random value v
```


这个技巧非常重要，如果不了解这个技巧就需要写全局变量，并且使用赋值，就不完全是函数式风格了。


---------------------

2024.11.12 更新

发现不太对。其实需要的是一个 state monad 这样的概念。state monad 也并不真正的好用，它只是把 state 隐藏起来了，还是需要全局的东西来传递，并不会消失。
