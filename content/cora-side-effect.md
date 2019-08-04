函数式语言的中赋值操作的实现模式

## cora 语言里面没有设计赋值操作

这篇博客是讲副作用的实现模式的。cora 是我设计的一门语言，我要写一下为什么这么设计，以及在其它函数式语言里面是怎么设计的。按是否偏好函数式风格划分，lisp 的实现可以分为两派。支持赋值的，以及不支持赋值的。函数式强调无状态，是不支持赋值的。

在 cora 语言里面，set 是一个函数，它的第一个参数是一个 symbol，第二个参数是一个 value，像这样子：

```lisp
(set 'xx 42)
```

语义是为符号 xx 绑定一个值 42，这个效果是全局的。也就是所有 set 调用都是全局的，它的意义是用来实现函数，也就是 defun 之类的宏，下面两者等价：

```lisp
(defun f (x) x)
(set 'f (lambda (x) x))
```

再有就是全局变量，比如：

```lisp
(setq *global* 42)
```

除此之外，基本不提倡使用。在 scheme 语言里面，有一个 `set!` 操作，是可以实现赋值的，在 cora 语言的设计里面，没有！也就是 cora 是一门函数式的 lisp。

但是...离开赋值操作没法好好写代码呀！(嗯，这也就是为什么从传统语言转到函数式语言的时候非常痛苦)

## 各语言里面是如何实现赋值的

让我们看一下其它语言里面，都是怎么样对待赋值这个问题的。

scheme 语言，`set!` 是一个特殊表，也就是支持赋值操作，并不追求纯粹的函数式风格。跟 common lisp 一样也多范式的，只是设计上面干净一点。

ocaml 语言，支持类型，但是对完全无副作用并不特别强调。可以定义 ref 类型。语法上面有一些糖，是 `:=` 和 `!` 底下的实现，类似于这种方式：

不支持

```
(set! a 3)
```

但是可以：

```
(let a (vector 3)
     (vector-set! a 0 3))
```

也就是说不支持修改变量绑定，但是可以把变量绑定到一个 box 里面，然后 box 的内容又是可以 modify 的。相当于开了一道口子。

shen 语言，也是重视纯函数式的。两种模式实现，一种也是像 ocaml 一样，用 `vector-set!`，另一种风格，是全局变量满天飞的使用 `set`。

haskell 纯函数式，无副作用。也是没有赋值的。它给出的答案是 monad。monad 理解起来还是挺烧脑的，这也是为什么函数式语言的程序员不够多的原因之一。

我们先排除掉 scheme，common lisp，以及 ocaml 那种不干净的方式，再去掉 shen 里面全局变量满天飞的方式。看下干净的方式是怎么样的：

链表求和，这属于还没上道的写法(非函数式风格)：

```lisp
(defun sum (l)
   (loop (sum 0)
         (if (null? l)
              sum
              (progn (set! sum (+ sum (car l)))
                     (set! l (cdr l))))))
```

新手 lisp 程序员这样写：

```scheme
(define (sum l)
    (if (null? l)
        0
        (+ (car l) (sum (cdr l)))))
```


有点经验的 lisp 程序员会这样写：

```shen
(define sum
    Res [] -> Res
    Res [X | Y] -> (sum (+ X Res) Y))

(sum 0 XX)
```

其实不光是尾递归的区别，注意，由于纯函数式没有赋值了，只能传参和函数调用，来实现赋值。也就是这里 Res 变量是一个累加值，通过传参数和尾递归，它其实担当了赋值操作的变量角色。

假设需要有多个累加器变量，也是全部写到参数里面的，比如收集链表中的奇数元素：

```cora
(func collect
    res idx [] => res
    res idx [x . y] => (collect [x . res] (+ idx 1) y) where (odd? idx)
    res idx [x . y] => (collect res (+ idx 1) y))
    
(collect [] 0 [1 2 3 4 5 6])
```

这里面的 res 和 idx 都是辅助参数。

## 多值返回

涉及到非尾递归，以及多值返回的时候，情况就有一些麻烦了。还是看下各个语言中的解决方案。

我们先看一下 C 语言是怎么处理多值返回的：

```
void f(int a, int b, int *ret1, int *ret2) {
   *ret1 = a + 1;
   *ret2 = a + b;
}
int ret1;
int ret2;
f(a, b, &ret1, &ret2)
```

通过玩弄指针的 trick，传参即返回值，然后传多个参数就可以实现多值返回了。

scheme 是在语言里面支持多值返回的，有 `values` 和 `define-values`，`let-values`，`call-with-values` 之类的东西做多值返回：

```scheme
(define (f a b)
    (values (a b)))
    
(let-values ((a b) (f 1 2))
    a)
```

至于实现方式，多半是给返回值在栈上打包之类的。

schen 语言里面的多值返回，是用 list 打包：

```shen
(define mult-values
    X -> [Ret1 Ret2])
```

由于有模式匹配，使用起来倒还方便。但这个方式比较丑，并且打包多了创建 list 的过程，有额外的对象分配开销，我不太喜欢。

haskell 之类的实现方式，也是类型打包，只是对象分配属于实现层面的东西。

```haskell
f : Ta -> a -> Tb -> b
```

用 shen 写出来就是这样子， `[tb B C]` 是类型为 tb 然后将 B C 打包了。

```shen
(define mult-values
    [ta A] -> [tb B C])
```

cora 里面怎么实现呢？ 我之前写过一篇 [scheme 中实现多值返回](multiple-returns-in-scheme.md)，cora 采纳的做法是一样的，用 cps。只不过我现在理解更深了。

```lisp
(f res1 res2 x ?)
```

假设 res1 和 res2 都是需要返回的值，我又不想用打包的方式 (cons res1 res2)，那么我们可以把这东西返回给一个 continuation：

```
(defun f (res1 res2 x cc)
    ...
    (cc res1 res2))
```

我们可以把 `cc` 重命名一下，让代码更好读一点：

```
(defun f (res1 res2 x return)
    ...
    (return res1 res2))
```

那么 return 的签名是 `(lambda (res1 res2) ...)`

还是拿之前那篇博客里面的例子，看代码怎么写

> 假设有个函数，它接受的参数是整数的list，返回所有list中的奇数构成的list，并返回所有的偶数之和，偶数之积。

```
(func demo0
      [] idx sum fac return => (return sum fac)
      [x . y] idx sum fac return =>
              (if (odd? idx)
                  (demo0 y (+ idx 1) sum (* fac x)  return)
                  (demo0 y (+ idx 1) (+ x sum) fac return)))
```

第一个参数是真正的输入，后面 `idx` 是辅助变量用于区分奇偶，`sum` 和 `fac` 分别是累计的和，累计的积，最后的 `return` 是用于多值返回的 continuation。封装一下，调用会方便一些：

```
(defun demo (l return)
  (demo0 l 0 0 1 return))
  
((demo [1 2 3 4 5 6 7 8 9]) (lambda (a b) a))
```

如果我们代码都是这样写，那么可以继续抽象一下多值返回的 pattern，或者定义一些宏简化代码，比如说支持 `let-values` 这样的宏：

```cora
(let-values xx (a b c) body)
```

其实就是改写成

```cora
(xx (lambda (a b c) body))
```

由于 cora 是自动 curry 的，像这种

```cora
(let-values (demo [1 2 3 4 5 6 7 8 9])
    (sum fac)
    sum)
```

展开就是

```
((demo [1 2 3 4 5 6 7 8 9]) (lambda (sum fac) sum))
```

其实存在 curry 就等价于：

```
(demo [1 2 3 4 5 6 7 8 9] (lambda (sum fac) sum))
```

完美！
