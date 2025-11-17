研究 [dependent type](/dependent-type.md) 之后，感觉没法拿过来用。实在是走到一个死胡同里面去了。我一直想给 cora 语言加类型，但不是像 shen 那样子加。主要是 shen 的类型系统的实现我始终都没能完全理解。

然后就想其它方式，比如说不用 prolog，而是用 [miniKanren](/micro-kanren.md)，然后也去研究过 miniKanren，发现以自己的掌握程度很难用它来替换掉 shen 的 prolog 部分，再实现上层的类型系统。

之前也研究过这个库 [latte](https://github.com/latte-central/LaTTe)，第一次没看懂。这次把 dependent 看过之后重新回顾一遍，发现大部分能理解了。然而却发现，这个也不是我想要找的答案。
latte 其实是实现了一个 λC 的语言，然后以此为内核，再实现出一个辅助证明的库出来。基本上做 proof 都是需要实现一个 dependent type 的内核的。这个库的目的不是为了给 lisp 加类型，而是利用 lisp 的 DSL 能力实现一个辅助证明的库。

有一点特别的地方是，我发现在类型推导这个领域，大家都是顺着推导的，或者说，思维模式都是正着的。比如说 dependent type 里面的那些 rule 吧，都是 `infer-type`，`check-type` 这种实现形式，其实正着思维很好理解。latte 那个库的实现也是这种形式：

```
having XXX prove by YYY
```

XXX 其实是类型的定义，然后 YYY 是函数的定义。由 Curry–Howard 同构，要证明 XXX 只需要找到一个函数，它的类型是 XXX 即可。库的实现就是用 dependent type 推导一下 YYY 的类型，再跟 XXX 判定一下类型等价，如果成功，就可以证明 XXX 了。
所以说到底，这个辅助证明它是由写代码的人，提供证明方式，一步一步顺着推导，证明出结论的。

shen 语言或者逻辑编程那边很不一样。它不是像普通的语言你告诉它怎么做...它是逻辑编程的，是用户告诉它想做什么，由它的逻辑引擎会推导出答案来。

不经过大量的 prolog 的训练真的很难跟得上这种思维模式，这也是为什么我一直无法啃透 shen 里面的类型推导的算法。
为了理解 type 我走了很多弯路了，仍然没有找到答案。这个周末甚至看 [koka](/koka-papers.md) 去找灵感去了（瞎点技能树，再次心疼一下自己)。

**静下来重新想了一下之后，我想到一个 idea。我可以把 shen 的类型的代码 port 过来，以库的形式给 cora 调用。**

上次已经实现了把 shen 编译到 klambda，再把 klambda 编译成 cora 语言，把 shen 编译成 cora 使用应该是可以的。
可以把 shen 的 prolog 的实现的代码直接编译出来，以库的形式提供。再然后，在其之上去实现 type。对于 shen 怎么样实现 prolog 的，我现在已经不需要再纠结了，把它当一个黑盒用就行。至于类型规则部分，就这么[300 行不到](https://github.com/tiancaiamao/shen-go/blob/57d62602ddefa1d3cbdccaa4918fd907581f067e/shen-sources-shen-22.3/sources/t-star.shen)，我基本上也能读懂，port 一下应该也没啥的。


另外，koka 的 algebraic effect 看起来也不错，应该也是可以再研究研究加到语言里面去的。


再 plus ... 最近 shen 语言论坛里面看到，[可能会有动作](https://groups.google.com/g/qilang/c/hfe7MV3MKi8)，如果作者不放鸽子的话。放了一个视频出来，shen 语言已经很久没有活跃更新的，作者基本上转向了闭源开发。不过这次，可能再次更新一个版本(Shen Professional)，这个版本会有不小的变化。还有 the book of shen 这次也会一起出第四版。再然后是，作者有提到这次会有一些库放出来，比如说 web 的，这样大家就可以用 shen 语言做一些实际的编程了。

