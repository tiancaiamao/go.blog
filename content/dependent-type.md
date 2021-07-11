
看了一些资料，感觉比较有价值的是这三篇：

- [Checking Dependent Types with Normalization by Evaluation: A Tutorial](http://davidchristiansen.dk/tutorials/nbe/)
- [Simpler, Easier!](http://augustss.blogspot.com/2007/10/simpler-easier-in-recent-paper-simply.html)
- [How to implement dependent type theory I](http://math.andrej.com/2012/11/08/how-to-implement-dependent-type-theory-i/)

第一篇 Normalize by Evaluation，就简称 NBE，前面部分讲得非常好，由浅入深。但是到了最后的真正讲到 dependent type 的部分，也就是 a tiny piece of pie 那一节，内容还是过于复杂了，不好理解。

这篇文章讲 Bidirectional Type Checking 讲得最好，至于 normalization by evaluation 那一块，我觉得其实只是实现 normalization 的一种方法，而用其它方法实现应该也是可以的，比如按大步语义做 subst 得到 normal form。或许用 normalization by evaluation 做 normalization 的真正好处，只是实现层面性能会有优势。但是对于易于理解的角度，并不 friendly。

至于 Bidirectional Type Checking 部分，其实对于理解 dependent type 不是必须的。NBE 这篇文章里面要做 Bidirectional Type Checking 的原因，是因为它在语法上面支持了 `(lambda (x) body)`，而不是 `(lambda (x T) body)`，即不写函数参数的类型。由于这里的类型缺失，实现就不能直接 syntax-directed 的方式了，只能 bidirectional。

最后一部分的问题是，它除了最基本的 lambda pi 之外，还引入了 sigma cons 一些其它的扩展类型，这对于一个不懂的人理解 dependent type 是没有帮助的，反而增加了阅读难度。

好在这篇文章讲解得很清晰，另外，最后面的补充材料部分也是对于理解整个领域非常有帮助的，因此仍然很值得推荐。


第二篇 Simpler Easier 结构更合理，内容更简单。其实不是说 NBE 不合理，可以理解为，它是讲如何用 NBE 这种方式来实现 dependent type 这个话题，而不单纯是 "如何最简单的实现 dependent type" 这样子的话题。Simpler 这篇文章的作者是看到另一篇 paper，叫 《Simply Easy! (An Implementation of a Dependently Typed Lambda Calculus》的论文，觉得写得还不够简单，有改进空间，于是写了这篇博客。

内容结构依然是由浅入，深层层递进的。第一小节是实现无类型的 lambda calculus，第二小节写了，叫 "有点绕道，simple type lambda calculus"，第三小节讲 polymorphic lambda calculus 作者评价是完全跑偏了。直到最后一部分，真正讲 dependent type。

理解 dependent type 并不需要通过理解 polymorphic lambda calculus 来过渡。反而是 polymorphic lambda calculus 里面用了更复杂的概念，参数具有类型，类型不再限于简单类型了(相对于 simple typed lambda)，类型也可以依赖于类型，于是类型那一层又引入了类型的 lambda 语法，类型叫 type，那么类型的类型叫 kind，kind 的类型又是什么呢？叫 star，可以形成三层的层级。这些概念其实是引入了不少理解复杂度的。只不可能读者本身是有 haskell 一类的语言基础的，haskell 也正是这样的三层，于是就没有那么陡的理解曲线了。但是对于没有 haskell 基础，这确实是走偏了。其实不要把类型分层，没有 type kind star 这样的概念，全部统一成 term，然后直接走到 dependent type，理解上可能是更简单一些的。

Simpler 这篇文章里面用的 weak head normal form，并选择了 lazy evaluation，这一点我并不太喜欢。总体来说，它的内容结构很合理，并且没有 lambda pi 之后的其它扩展的概念，这对于初学来说是很好的简化。

第三篇，这一篇是最合我味口的，它并没有从 untyped lambda calculus 开始讲。没有一点废话，不服就干，直接上代码。我就喜欢这种风格。然后代码也很简短。

原本实现是 ocaml 语言的，我在边看边学习，把它的代码 port 到了 lisp，上代码 https://github.com/tiancaiamao/cora/blob/dd1f6c0efe53dc2e2a78601507dbec68b1ab1211/lib/dt.cora

接下来的内容是一点点笔记了。

`pi` 类似于 `lambda` 是一个新的 binding，或者也可以叫 `forall`，这个 binding 的形式是 `(pi (A T) B)`。 其中 A 是变量， A 的类型是 T，然后返回的类型是 B， B 里面可以出现 A，也可以不出现。 `A -> B` 这个就等价于 `(pi (_ A) B)`，如果在 B 中 A 不出现。

为什么要 normalize？ 因为需要判断类型等价。在 simple type lambda 里面，类型就是具体某一种具体类型，判定等价可以直接用等号。而到了 dependent type 里面，类型也可以是依赖 term 的，类型有可能包含 lambda 了，于是要考虑两个 term 的等价，相当于 lambda 表达式判断等价。比如说  `((lambda (x T4) x) T3)` 这个跟 `T3` 其实是等价的。所以有 normalize 这一步，先转到规范型再判等。先做 beta 变换，把 redux 都干掉。再判断 alpha 等价。

怎么实现 beta 等价？ NBE 里面的做法是先 normalize 到特定的表示，并不走完全部的 subst，所以会引入 neutral，如果 neutral apply 到另一个东西，那整个 apply 表达式就再变成另一个 neutral。NBE 里面通过先 normalize 再 read back 实现了简化过程。第三篇的做法是按 beta 变换的语义直接做完整的 subst。 `subst x t e` 即把表达式 e 里面的 x 变量全部替换成 t。

怎么实现 alpha 等价？

```
(func alpha-eq
      env x1 x2 => (= x1 x2) where (and (symbol? x1) (symbol? x2))
      env [f1 x1] [f2 x2] => (and (alpha-eq env f1 f2) (alpha-eq env x1 x2))
      ['lambda [x1] B1] ['lambda [x2] B2] => (lambda z (gensym 'xxx)
						      (alpha-eq env B1 B2))
      _ _ => false)
```

这里面的关键是 lambda 那个规则，将两个表达式的参数，都用同一个不冲突的符号替换掉，然后在新的环境里面匹配两个 body 部分。

Bidirectional Type Checking 的目的？ 前面已经提了，如果 lambda 参数里面不带类型信息，就需要 Bidirectional 了。如果是 `(lambda (x T) b`，我们可以很容易实现 findType 函数：

```
(func findType
	...
	ctx ['lambda [x T1] b] => (let T2 (findType (extend ctx x T1) b)
								['-> T1 T2]))
```

但是没有 `x:T1` 这样的信息的时候，就没法做 `(findType (extend ctx x T1) b)`。
