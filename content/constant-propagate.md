遇到这个问题是这样的，`select * from t where a < 5 and a > 5` 我期望它能直接推导出来，这是一个空集，因为 `a < 5 and a > 5` 恒为假。结果呢？它给我搞出来了一个全表扫！这不就傻逼了。

嗯，扫了一眼我们的代码，开始没看太懂。于是决定自己想想怎么做。比如我们有下面这一系列的条件：

[= a 1] [= a c] [> b c] [= b d] [> c d] [= d f]

那么我们需要做的是，把 `a = 1` 这个常量表达式，不停地拿到集合里面去做替换，替换过程中，如果有新的常量表达式生成，可以记录下来。`[= a c]` 这个替换掉之后，就会生成一个 `c = 1`，继续用它执行下去，可以得出 `[> b 1]` 和 `[> 1 d]`，像 `[= d f]` 这种消除不掉的，就继续保留下来。

那么这个算法的基本框架是这样子的，已知的常量表达式 `[= a 1]` 这种，放到一个叫做 `Rules` 的集合里面，而未知的待替换的集合，我们叫 `Input`，拿 `Rules` 里面的每一条，去 `Input` 里面做替换，过程中如果有新的 `Rule` 生成，则放到 `Rules` 集合里面。直到 `Rules` 集合的所有规则都使用过了，没有更多的替换时，算法结束。

为了体现 lisp 的优越性，我决定用 shen 语言写写这段代码（装个逼）。


    (define constant-propagate0
            [] Input -> Input
            [Rule | Rules] Input -> (let Pair (apply-subst Rule Input [] Rules)
                                        Output (hd Pair)
                                        Rules (tl Pair)
                                        (constant-propagate0 Rules Output)))
                                        
这是一个递归函数。第一行，如果 `Rules` 为空了，算法就结束了。第二行，取第一条 Rule，将它应用到 `Input` 集合所有成员 `(apply-subst Rule Input [] Rules)`，它会生成新的 Rules 和新的输入（这里叫 `Output` 了），递归继续做 `constant-propagate0`。

    (define apply-subst
        Rule [] Output Rules ->  (cons [Rule | Output] Rules)
        [= Var Const] [[= Var Var2] | Input] Output Rules ->
              (apply-subst [= Var Const] Input Output [[= Var2 Const] | Rules])
        [= Var Const] [[= Var1 Var] | Input] Output Rules ->
              (apply-subst [= Var Const] Input Output [[= Var1 Const] | Rules])
        [= Var Const] [[OP Var Var2] | Input] Output Rules ->
              (apply-subst [= Var Const] Input [[OP Const Var2] | Output] Rules)
        [= Var Const] [[OP Var1 Var] | Input] Output Rules ->
              (apply-subst [= Var Const] Input [[OP Var1 Const] | Output] Rules)
        Rule [H | Input] Output Rules -> (apply-subst Rule Input [H | Output] Rules))
        
`apply-subst Rule Input` 函数，第三个第四个参数，`Output` 和 `Rules` 是为了尾递归才写成这样的，它做的事情就是用 Rule 去跟 Input 里面的各个比较，新的结果会放到 `Output`，如果有新规则生成会放到 `Rules`。

好了，在 [shen 语言](http://www.shenlanguage.org/) 里面（随便找个实现，比如 [shen-go](https://github.com/tiancaiamao/shen-go)），敲进去这两函数，然而执行

    (constant-propagate0 [[= a 1]] [[= a c] [> b c] [= b d] [> c d] [= d f]])
    
会得到

    [[= c 1] [> b 1] [= b d] [> 1 d] [= d f] [= a 1]]

其实这个算法是借鉴类型推导里面，一个叫做算法W 的做法，简化的版本（好吧，我根本不知道常量传播标准做法应该是怎么做的）。

有几个问题，这里只处理了 `var = const` 这种表达式的推导，这是最简单的情况。其实归纳下来有好几种更复杂的场景。

`var1 = var2` 和 `var > const`

这种情况其实不难，继续套用上面的算法框架仍然是能够处理的。比如我们只把 `var > const` 这一类，作为 `Rules` 集合，把 `var1 = var2` 当作 `Input` 集合，拿规则集里面每条规则，过一遍 Input，就可以推导出更准确的 var 范围。

像之前的 [= a b] [= b c] [> a 5] [< c 3] ，先从 `Rules` 里面取出 [> a 5]，将它应用到 [= a b] 上面，推出 [> b 5]，新的 [> b 5] 被加到 `Rules` 里面，再应用它到 [= b c] 时，会推出 [> c 5]，丢到 `Rules` 里面... 最后规则里面会出现矛盾 [< c 3] [> c 5]，这种场景我们就可以判定 false 了。

比较难处理的，是 `var1 > var2` 这种场景的推导。当 `var > const` 作用于它时，结果不那么直观。比如 `a > b`并且 `a > 3` ，这个得不出啥信息。但 `a > b` `b > 3`，这个可以推出 `a > 3`。也是可以丢到规则集里面继续利用的。主要麻烦在场景复杂了一些。

目前就想到这些吧。说说一个同事的脑洞。

1. 如果把所有等号两边的点，都当做图上面的节点，然而 `a < b` 就构造一条从 a 到 b 的有向边。比如 `[< a b]` `[> a 5]` `[< b 4]`，会构造出 `a -> b` `b -> 4` `5 -> a`，如果我们再手动用 `[< 4 5]` 构造一条，`4 -> 5`，那么这个图就会成环了。也就是说，通过这样子构造一个图，然后检测成环，可以判断表达式是否是恒假的。

2. 如果出现更复杂的 `a - b < 3` `b - c < 5` 这种表达式，两者相加可以推出 `a - c < 8`。继续假想一个图，从 a 到 b 画一条出边，边的权值为 3，含义就是 a 到 b 节点的距离最少是 3。然后 b 到 c 画一条出边，边的权值为 5。把所有这种表达式都构造成图，那么求任意两节点之间的最短路径，就是两个变量的差值。比如 a 到 c 最短路径是 8,就是 `a - c < 8`。如果再把常量引入，有可能把各变量的一些范围信息推出来。

暂时还没有更多的想法，这哥们好喜欢图。好晚，不知道自己在想啥了，明天先好好改代码 bug 吧，不脑洞，实用为主。
