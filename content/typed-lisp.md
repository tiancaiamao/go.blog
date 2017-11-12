在我接触过类型之后，就觉得这是好东西，lisp里面没有真是太可惜了。于是就一直在思考：如何为lisp加类型，才是合理的方式。这一块水太深了，搞了好久也没算搞透彻，这里算是记录一些碎碎念。

1. HM
2. 类型即约束
3. 宏和逻辑引擎

## HM

谈及类型系统肯定会说到大名鼎鼎的 Hindley–Milner，第一个要说的就是HM。

为什么HM是最流行的一个类型系统呢？我觉得它有一些特别的优点：简单，soundness，并且decidable。

soundness是说，如果一个类型系统的规则推导出来的类型，跟程序真实执行得到的值的类型是一致的，那么这个类型系统就是soundness的。soundness也就是指正确性，如果能保证soundness，那么只要通过了类型系统的程序就不会出错(get stuck)。

decidable是说，一个类型系统的规则是可以求解的。举个极端的例子，如果要知道程序的类型，需要真实地把程序执行一遍，程序可能是死循环的，那这样的做法就不是decidable的。

HM非常基础，然后能保证是soundness的。然后它是decidable，计算复杂度还很低。用大O表示复杂度是O(?)忘记了。所以像ML，Haskell的类型系统基本都是在HM之上的变种，Haskell做的可能更激进一些。

为什么使用HM或者照搬ML，Haskell的类型并不适合lisp？

本质上是哲学问题。lisp追求的是灵活性和表达能力，而类型则是正确性，会限制一些“可能”正确的程序。

举个例子，这个表达式：

    (lambda (x)
      (cond
      ((= x 3) 42)
      ((> (strlen x) 5) #t)))

输入参数x的类型可能是 `integer|string`，而返回值的类型可能是 `integer|boolean`，并且还是有关联的，整个函数的类型则是 `integer->integer|string->boolean`。
   
HM系统不让这么干，如果写类似的东西必须加tag并定义类型。要求输入以及返回值的类型都是确定的，通过规则的约束，保证写出来的代码都是对的，但是也误杀了一些“其实是”对的程序。这跟lisp追求的灵活性的矛盾，限制了表达能力。

tagged union必须是正交的。如果非正交，就需要子类型判等。实现HM的时候，用tag的判等比较好做，然而有子类型，把等于换成集合的属于关系，就很难处理了。

另外还有宏的问题，宏可以把一门语言完全变成另一门语言，带宏了之后，跟支持类型系统也是很冲突的。

如果生硬地模仿ML或者Haskell的东西，我觉得这个方向是不对的。语法会特别丑，而且怪怪的，这种做法给lisp加类型还不如直接换语言呢。反面教材参见[hackett](https://github.com/lexi-lambda/hackett)，和[lux](https://github.com/LuxLang/lux)。


## 类型即约束

接下来要讲的是第二个点，类型即约束。

彭飞写过一篇《王垠，请别再欺负我们读书少》，王垠反击写了一篇《到底是谁欺负谁读书少》，那一次的PK堪称惊天地，泣鬼神。最后以王垠完胜，彭飞删除了原文收场。神仙打架，我们凡人看戏。[知乎上还有人提问](https://www.zhihu.com/question/42315543)：PLT零基础的人，要看懂王垠和彭飞在《王垠，请别再欺负我们读书少》里讨论的内容，需要掌握哪些知识？

我的态度，怎么说呢？拒绝看不懂以下公式的人讨论王垠的水平：

![](http://upload-images.jianshu.io/upload_images/68562-eae6c6cd2eecfb4a.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/500)

> [王垠：每一个函数的类型，就是这个函数本身。没有比函数本身更能够描述函数的特征的类型](http://yinwang0.lofter.com/post/183ec2_c9184be)

这观点真是让我眼前一亮，尤如醍醐灌顶一般。对于lisp，从实用而不是学术的角度，并不需要特意去追求类型系统是sound的，它只要能帮助我们检测出一些错误，这就够了。这个套路就是，类型即约束，我们并不需要知道某个东西确切的类型，而只是知道哪些使用方式违反了约束，是不安全的。

王垠的做法走抽象解释，更类似静态分析而不是类型推导，把程序虚拟的跑一遍，然后去发现各个表达式的类型，这个过程中关于类型使用时的约束信息就建立起来了。

跟着知乎的链接，我发现更早就有一些论文研究，比如soft typing scheme。比如Erlang的Dialyzer/Typer。

实际上HM在实现tagged union扩展的时候，就已经在推导规则里面不可避免地引入了一定程度的约束系统。如果允许子类型之后，约束扮演的角色就相当重了。

如果我们检查到可能出现了类型错误，并不是拒绝掉程序，而只是警告，这其实是非常有用的。可以在类型安全跟程序的灵活性里面取得一个平衡。这个方向对于给lisp加类型绝对是更可取的做法。

## 宏和逻辑引擎

lisp没有类型，怎么办？忘记哪天网上闲逛看到一个观点：真正的lisp高手答案都是logic engine。起初没有引起重视，直到后来遇到shen语言，才真正的理解了这句话，然后费了老大的劲找到原出处。

> [fogus："Macros and logic engine versus type system... to the death."](https://news.ycombinator.com/item?id=2595036)

fogus 这哥们对于lisp还是非常有见地的，不管是[lisp爱好者](http://blog.fogus.me/2011/05/03/the-german-school-of-lisp-2/)，还是[非lisp爱好者](http://blog.fogus.me/2011/10/18/programming-language-development-the-past-5-years/)，都可以看一看。

给lisp加类型，如果从实用角度出发，我们可以把soundness/decidable都给划掉，不sound也没关系，能帮我们发现错误就好；不decidable也没关系，写代码的时候都repl的，只需要做到如果跑不出来，那一定代码写的有问题。于是，我们从两个角度来评判给lisp加类型：

* 表达能力 
* 复杂性 (实现，使用)

leslie lamport 老爷子大家都熟习吧？搞分布式领域，如果没听过这位老爷子的，建议尽早改行。用他的话说，就是当年他研究分布式领域的时候，比尔盖茨还穿着开裆裤在玩泥巴！我无意之中发现，他还写过类型方面的文章：Should Your Specification Language Be Typed? 这篇文章的主张是用集和论来表达类型。类型系统跟集合论，跟逻辑学，都有着莫大的联系。关于逻辑，计算的还有一个[柯里-霍华德同构](https://zh.wikipedia.org/wiki/%E6%9F%AF%E9%87%8C-%E9%9C%8D%E5%8D%8E%E5%BE%B7%E5%90%8C%E6%9E%84)。

比如像用集合论来表示类型，这想法就是很新颖的，因为表达能力不再受限在HM的框架，复杂性可能就有点高了。

用逻辑引擎的做法在表达能力是上无与伦比的。以shen语言为例，我在[知乎上回答](https://www.zhihu.com/question/60702229/answer/213029957)过，这里搬过来：

为了说明它的先进性，让我们看一个例子。如果我要定义一个集合类型set，该怎么办？集合是这样的，它里面没有重复的元素。

    (datatype set
                ____________
                [] : (set A);

                if (not (element? X Y))
                X : A; Y : (set A);
                ================
                [X | Y] : (set A);)

这段代码是说，空[]算是一个集合；

如果Y是一个集合，如果X不是Y里面的元素，那么，`[X | Y]`也是一个集合。

    [1 2 3] : (set number)
    ["sasdf" "bc"] : (set string)
    [1 1 2] : type error
    ["aa" 1] : type error

shen是通过手写相继式演算(Sequents Calculate)来定义类型的。datatype里面全部是一些Horn Claus，来指定什么情况下可以证明一个东西是某种类型。这里有一篇相继式演算的[教程](http://logitext.mit.edu/logitext.fcgi/tutorial)。

这种方式表达能力是完胜的。使用复杂性方面，描述基本的类型比HM那一套稍复杂，但是比起在lisp里面强加一套表示类型的语法，还是有优势的。实现方面，datatype其实是lisp宏，sequent rules会被先编译成的类似prolog语言，然后编译到kernel lambda。

我觉得这个做法跟lisp简直是的绝配！ 论文的话，POPL 17的 Type Systems as Macros，也用到了这个方法，DSL定义规则，加逻辑引擎实现。
