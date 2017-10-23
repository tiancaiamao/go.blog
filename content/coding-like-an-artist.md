## 1. 简约而不简单

优雅的代码是一门艺术，更多取决于品味。我喜欢简短，可读的代码。完成同样的功能，使用的代码越短这份代码越优雅。有些人说，我也喜欢写简短的代码，你看：

    while (*s++ = *t++);
    
这种“优雅”作为教科书级别的经典，实在教坏了不少人。且不说前缀加加和后缀加加的语法设计糟点，这里副作用的操作还依赖返回值，我只能呵呵。然而有些人还特别喜欢抠语言的边边角角，以此号称精通某语言，一股阿Q的“茴香豆的茴字有三种写法”的画面就映在脑中。

还有些人喜欢在if后面省略大括号来少几行代码，或者在有些地方省略else语句以减少几行代码。比如：

    if xxx
      doSome1();
      doSome2()

第二个语句本来是满足条件才执行的，结果就直接执行了。又或者这种：

    if xxx {
      if yyy {
          doSome1()
      } else {
          doSome2()
      }
    }
    
想着把条件合并了少几个分支少二行代码：

    if xxx && yyy {
        doSome1()
    } else {
        doSome2()
    }

在xxx为假的条件是不应该执行doSome2的，在这种地方玩小聪明特别容易翻船。这些都不是我说的那种简短和优雅，优雅的代码首先需要是正确的。

顺便说句，从语言设计的角度，我是支持if必须写全else分支的。为什么呢？在函数式语言里面，没有statement只有expression，所有expression都应该有一个返回值，返回值是要有类型的。对于if，它的then分支和else分支的类型应该是相同的。然而如果if不写else分支，试想这种：

    (set! a (if xxx 1))

由于只写了then分支没写else分支，if的返回类型是未知的，到底是int还是空类型呢？于是a的类型也是未知的，这对类型推导很不好。类型系统对于程序的正确性非常重要，so，我支持if不能省略掉else条件。

当然，这只是针对我自己认为合理的语言设计而言。对于像Go这类传统的语言，那么建议做法是跟着标准风格走。标准的风格应该怎么写呢？**如果要省略else分支，应该将异常分支写到前面，然后用return尽早退出**，像这样子省略else分支：

    if err := xxx; err != nil {
        handle(err)
        return
    }
    if err := yyy; err != nil {
        handle(err)
        return
    }
    
反例写出来是很丑的：

    if err1 := xxx; err1 == nil {
        if err2 := yyy; err2 == nil {
            doSome()
        } else {
            handle(err2)
        }
    } else {
        handle(err1)
    }
    
## 2. 借鉴好的东西
    
不过Go语言里面error处理确实不太好，到处充斥着这样的代码，还是很丑的。让我们思考下如何让错误处理稍微优雅一点。

下面这个例子，假设我们要从数组里面Unmarshal一个数据结构出来：

    func (l *mvccLock) UnmarshalBinary(data []byte) error {
        buf := bytes.NewBuffer(data)
        err := ReadNumber(buf, &l.startTS)
        if err != nil {
            return errors.Trace(err)
        }
        err = ReadSlice(buf, &l.primary)
        if err != nil {
            return errors.Trace(err)
        }
        err = ReadSlice(buf, &l.value)
        if err != nil {
            return errors.Trace(err)
        }
        err = ReadNumber(buf, &l.op)
        if err != nil {
            return errors.Trace(err)
        }
        err = ReadNumber(buf, &l.ttl)
        if err != nil {
            return errors.Trace(err)
        }
        return nil
    }
    
错误处理的逻辑跟正常逻辑是混在一起的，造成阅读干扰。如果定义一个辅助结构，然后把Read实现成它的方法，把error记录成它的内部状态：

    type marshalHelper struct {
        err error
    }
    func (mh *marshalHelper) ReadNumber(r io.Reader, n interface{}) {
        if mh.err != nil {
            return
        }
        err := binary.Read(r, binary.LittleEndian, n)
        if err != nil {
            mh.err = errors.Trace(err)
        }
    }

于是我们就可以这样写了：

    func (v *mvccValue) UnmarshalBinary(data []byte) error {
        var mh marshalHelper
        buf := bytes.NewBuffer(data)
        mh.ReadNumber(buf, &v.vt)
        mh.ReadNumber(buf, &v.startTS)
        mh.ReadNumber(buf, &v.commitTS)
        mh.ReadSlice(buf, &v.value)
        return errors.Trace(mh.err)
    }
    
注意到没有？丑爆了的error处理消失了，这就优雅多了！这段代码其实是我从项目里面拿的一个真实例子，[在这里](https://github.com/pingcap/tidb/blob/d7a92e1e2380e2159f9a01b3efa00f275a0da96e/store/tikv/mock-tikv/mvcc.go#L73)可以看到。

如果熟习函数式语言并且了解monad的话，其实明眼人是可以看出来的，这里本质上是使用了函数式语言里面的monad的技巧。返回值的类型是

    data result = None | Error of error
    
然后让错误处理走另一条通路了。想了解更多可以看看[这里](https://fsharpforfunandprofit.com/rop/)，顺便说句，F# for fun and profit这个系列写的真心不错，强力推荐。

## 3. 语言影响思维模式

函数式语言真的是好东西，无论是否使用，都应该看一看，对于写优雅的代码很有帮助。这里的错误处理只是一个例子。很多东西只有在见过之后，大脑才能形成概念，直到有机会把概念用到其它的地方，触类旁通。parser combinator在函数式语言里面是一个非常经典的教程，由于我之前看过，有一次写Go的时候就看到了启发。场景是这样子的，kv存储引擎里面存了mvcc的信息，数据的布局类似这样子：

	// Key layout:
	// ...
	// Key_lock        -- (0)
	// Key_verMax      -- (1)
	// ...
	// Key_ver+1       -- (2)
	// Key_ver         -- (3)
	// Key_ver-1       -- (4)
	// ...
	// Key_0           -- (5)
	// NextKey_lock    -- (6)
	// NextKey_verMax  -- (7)
	// ...
	// NextKey_ver+1   -- (8)
	// NextKey_ver     -- (9)
	// NextKey_ver-1   -- (10)
	// ...
	// NextKey_0       -- (11)
	// ...
	// EOF
    
需要从goleveldb里面读取这些东西，解析需要的部分，并且根据是否遇到锁以及版本信息做一些额外的处理。整个代码开始是写的比较恶心的，因为访问要操作一个iterator，然后有一些中间状态的需要存下来，当iterator遍历的操作跟数据处理逻辑耦合之后，代码就很丑了。后来突然灵光一闪：这不是正是可以从parser combinator借鉴的场景么？interator抽象成一个stream，我只需要定义很多小的"parser"，这里是decoder，每个decoder只做一点点事情，比如只解析一下lock数据的decoder，比如只解析一个mvcc value数据的decoder，甚至什么都不解析，只将iterator移到下一个mvcc记录的decoder。然后将它们组合使用，就可以发挥无穷威力。

定义一个interface，所有的decoder都是实现这个接口，状态相关的存到实现的decoder内部。

    type iterDecoder interface {
        Decode(iter *Iterator) (bool, error)
    }
    
函数可以组合但是不能记录状态，而对象是记状态不易组合。monad本质是上把状态记录到了类型里面，来实现带副作用的。

我从函数式语言那边受了很多启发，很多时候它都教会我写更好的代码。当然，面向对象也有教我很多东西。

之前我们的[项目里面有一个IndexLookUpExecutor](https://github.com/pingcap/tidb/blob/a264f81acc1e69d3f20cb2815873e7659843e675/executor/new_distsql.go#L280)，这个东西做的事情是由多个goroutine并行地从数据库里读索引，解析索引数据，再利用索引数据里面解析出来的信息去并行地读数据库的表数据。这块代码很乱，经常出各种bug，比如退出有goroutine泄漏，或者关闭channel时卡死。一方面原因是逻辑比较复杂，涉及到goroutine的并行，并且状态比较多。另一个更重要原因，就是代码写的烂（陈年老代码，经过很多人的手，逻辑复杂，不敢动，逢改必引入bug，这种代码最让人咬牙切齿）。怎么重构它呢？我仔细分析了复杂性的原因：数据结构内部维护了太多状态。

那么基于面向对象的思维，将它拆小，IndexLookUpExecutor被做成了indexHandler，tableHandler，每个对象只管理自己的内部状态，goroutine相关的控制也移到对象内部。IndexLookUpExecutor不再关注对象内部的状态，通过方法来交互indexHandler和tableHandler。对象各自管理自己的内部状态，并且状态小了之后，一下子就可维护了。

其实都是很老生常谈的概念：**模块化。一个函数只做一件事情。高内聚，低耦合。对象内部状态不需要暴露给外面，通过对象的方法交互**。无关设计模式，只是最基本的道理。面向对象的重要道理就是，把状态控制在对象管理的范围之内，函数式那边的答案是类型。

最好不要成为只精通某一门语言的专家（尤其不要是C艹语言），无论是过程式的，函数式，面向对象的，都多了解一些，有助于知道在合适的时候该使用什么方式才是合理的。语言决定了思维模式，这是真的。拿Go语言来说，如果一直停留在锁这种底层的东西上面，其它语言里即使自己实现一些机制，也没法像goroutine/channel体验到CSP/Actor之些高层思维带来的酸爽。有了语言内置的支持以后，做并发编程的思维完全就进入到了一个更高层的抽象。


## 4. 抽象再抽象

记得SICP那本书里面，一直在教人们抽象再抽象，其实这是写好代码非常重要的一环。在编程语言里面，处于抽象的顶点的概念是什么呢？递归。迭代是人，递归是神！

我们知道，面试的时候可能会要求写一个链表反转，默认会用C语言之类的低级语言。然而如果语言可选，我会拿lisp糊他一脸，你丫的傻逼面试官，教你重新写代码：

    (define reverse
        []      -> []
        [X | Y] -> (append (reverse Y) X))

简洁，易读，bug free，这才是优雅呀！

再说一个排序的例子，我不想说快排，太欺负C语言了。就说冒泡排序吧，正好可以看个不动点的例子。

    (define fix-point1
        F X X -> X
        F X Y -> (fix-point1 F (F X) X))
    (define fix-point
        F X -> (fix-point1 F (F X) X))
        
不动点就是，X=F(X)。一个函数对参数执行之后，得到的结果还是那个参数。

    (define bubble
        [] -> []
        [X | Y] -> (bubble1 X Y))
    (define bubble1
        X [Y | Z] -> [Y | (bubble1 X Z)] where (> X Y)
        X [Y | Z] -> [X | (bubble1 Y Z)])
        
我们看bubble1这个递归函数，它的功能是将链表冒泡一遍。X和Y分别是链表当前的头节点，如果X大于Y，则交换位置。递归的做这个动作，交换两节点，做一遍就冒泡了一遍。亮点来了...
        
    (define bubble-sort
        X -> (fix-point bubble X))
        
一遍一遍地冒泡，直到达到一个不动点，排序就OK了。这是我看到过的最漂亮的代码之一！嗯，shen语言看到的例子。

我给过很多人很高的评价，给过很多项目的代码很高的评价，比如常提到在贵司里面[@coocood](https://github.com/coocood)写代码还是挺有节操的，还有包括[@iamxy](https://github.com/iamxy)也是，虽然很少写代码了。再比如云风代码非常值得学习下，还有王垠的代码也是写得相当漂亮的（可惜都删了）。一些项目，像lua的源代码，skynet源代码等等都是很值得学习的。但是若说我见过的最优雅的代码，还是[shen语言](http://www.shenlanguage.org/)的源代码。我不止一次推崇这份代码了----这真的是艺术家的作品！

## 5. 不要被表达能力束傅

写代码是一门艺术，对于艺术家来说，代码的表达能力非常重要。语言的表达能力强，就可以用更简洁的代码完成复杂的功能，代码也就更加的优雅。语言既影响思维方式的，也能阻碍表达的。有些东西在这门语言里面体会不到，非得另一门语言里面才能，这便说明语言同时阻碍了表达。

表达能力的极致是什么呢？lisp的宏。这是唯一一种不阻碍表达的方式。很多人不喜欢lisp的语法，其实lisp根本没有语法。真正的lisp程序员会创造属于自己的lisp，只有自己按自己的语法表达，才是随心所欲。shen语言真正做到了这一点。

最初看到 |> 和 >>= 这些符号的时候，简直被惊艳到了。中缀运算符的能力非常强大，像这样写代码：

    x
    |> add 1
    |> times 2

后来发现其实这个操作其实用函数是可以完成的，不需要中缀运行符：

    (fun compose
        X [] -> X
        X [F | Fs] -> (compose Fs (F X)))
    (compose x [(add 1) (times 2)])

看到模式匹配，觉得是个好东西，其实lisp早就能支持了，一直没注意而已。看到类型系统时觉得，这么好的东西，lisp能不能吸收进来呢？我在shen语言那里得到了答案。

曾经看过一个20几万行的游戏服务器的代码，真的很不优雅。按理说它完成的功能根本没必要那么大的代码量。原因是它有近一半的代码，全是在处理发包解包，全部手写硬编码的。那个年代还没有protobuf，没有grpc这些东西。假如有DSL，有代码生成器，那这一半的代码都可以干掉了。重复，低效的劳动。

后来看到shen的YACC惊呆了，100多行不到200行的实现！最惊讶的是它的类型系统居然是用生成代码的方式实现的，只实现了一套逻辑引擎，然后生成类型推导的规则了喂给逻辑引擎，一套类型系统就搞定了。整个项目4000多行代码，在我看来比那20万行代码要多得多。
