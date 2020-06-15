最近学习 miniKanren，把《the reasoned schemer》看了一下。然后是看了下 microKanren，这篇 paper [µKanren: A Minimal Functional Core for Relational Programming](http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf) 做了一个很好的介绍。

单纯看书没用，要吸收知识，非要[代码写一遍](https://github.com/tiancaiamao/cora/blob/d59c3f701c96789d6d239845c674c8ceab09230b/lib/ukanren.cora)，博客写一遍，才能够理解，于是整理一下笔记。

**理解 microKanren 最重要的是，先知道整体的概念，也就是 state goal stream 这些东西。需要把定义多研究几遍，直到理解概念。**

### state

state 是由一个 substitution 和一个 free variable 计数器构成。

substitution 是没有做去重的，关联链表。每一项就是一个 `[var . xxx]` 这样的形式。xxx 可以是另一个 var，也可以是一个 value。

var 就是 vector 包装过的一个结构：

```
(defun var (x)
    (let v (vector 1)
         (vector-set! v 0 x)))
```

### goal

goal 是一个函数，接受一个 state 返回一个 stream

### stream

stream 就是一串的 state。microKanren 返回的最终结果就 stream


**理解了 goal 和 state 等概念之后，再理解最核心的四个操作就够了，分别是 == fresh conj disj，它们都是构造 goal 的。**

### ==

== 是最基础的构造 goal 的方式。两个东西是否能 ==，就是它们是否能 unify，所以 == 的实现其实就是 unify。

```
#1 (defun == (u v)
#2    (lambda (state)
#3        (let subst (car state)
#4             free-var-idx   (cdr state)
#5             (let s1 (unify u v subst)
#6                  (if (null? s1)
#7                      ()
#8                      (cons [s1 . free-var-idx] ()))))))
```

返回值是一个 goal。goal 是接受一个 state 并返回一个 stream 的函数。所以是第 2 行

```
(lambda (state)
```

第 3 第 4 行是解构 state，state 是由一个 substitution 和一个 free variable 计数器构成。

unify 的结果是一个新的 substitution。所以第 5 行的 s1 是一个 substitution。

第 7 第 8 行是返回结果，结果是一个 stream。stream 是啥？是一串的 state。
所以第 8 行 `[s1 . free-var-idx]` 返回的是一个 state，而最终返回的是一个只有一个 state 的 stream。

### fresh

fresh 用于引入新的 var。

fresh 接受一个函数 f，返回一个新的 goal。在那个函数 f 里面，state 是对应了新的逻辑变量的。

```
#1 (defun call/fresh (f)
#2    (lambda (state)
#3        (let subst (car state)
#4             c (cdr state)
#5             (let new-goal (f (var c))
#6                  new-state [subst . (+ c 1)]
#7                  (new-goal new-state)))))
```

第 5 行，f 接受新的逻辑变量绑定，在新的作用域内执行。执行结果是返回一个新的 goal。

第 6 行，我们需要给新的 goal 提供新的 state，在该 state 里面，`(var c)` 是引入的新的逻辑变量。

`call/fresh` 的返回是一个 goal，然后 goal 是接受一个 state 并返回一个 stream。所以在最终的返回是一个 stream。第 7 行的新的 goal 作用于新的 state，也就是返回的 stream 了。


```
(set 'empty-state (cons () 0))
((call/fresh
    (lambda (q)
        (== q 5))) empty-state)
```

看一个具体例子，`call/fresh` 返回一个 goal，对这个 goal 调用 empty-state 最后得到一个 stream，得到结果是 ((((#0 . 5)) . 1))


### conj 和 disj

conj 和 disj 都是接受两个 goal，返回一个新的 goal。只不过 conj 表示“且”的关系，两个 goal 都要成立，而 disj 表示“或”的关系，其中一个 goal 成立即可。

```
(defun disj (g1 g2)
    (lambda (state)
        (mplus (g1 state) (g2 state))))
```

mplus 用于将两个 stream merge 起来，返回一个新的 stream。

后面的一个扩展是，因为 stream 可能是无限的，所以如何表示一个无限的东西，用 lazy 呗。做一个 eta 变换

```
#1 (func mplus
#2      [] x => x
#3      x y => (lambda () (mplus y (x))) where (procedure? x)
#4      [h . t] z =>  (cons h (mplus t z)))
```

其中第 3 行的条件，如果 stream x 是一个无限流，则做 eta 变换。

一些非核心本质的东西，像搜索策略，reify 之类的。microKanren 跟 miniKanren 不同的

**如果是 microKanren 到这里已经基本够了。如果是 miniKanren，再然后是几个宏包装一下子**

### `Zzz` `conj+` 和 `disj+`

Zzz 宏就是做一个 eta 变换。

```
(defmacro Zzz
    (lambda (input)
        (let goal (cadr input)
             state (gensym 's)
	     ['lambda [state]
                  [goal state]])))
```

由于 conj 和 disj 都是接受固定参数，这两个宏把它们扩展成接受变长参数。

这几个都是辅助实现的，不属于最终的对外接口。

### conde fresh run

conde 和 fresh 分别是包装一下 `disj+` 宏和 `call/fresh` 函数。

最后 run 的实现要做 refier。refier 留给用户层了，在 microKanren 里面算不上核心的东西。实现也简单，就是

```
(reify (take n (g empty-state)))
```
