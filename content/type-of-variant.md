说起类型系统，都知道大名鼎鼎的hindley-milner类型系统。HM类型系统不算太难，资料也是一大堆，几乎是变成了类型系统的代名词。我觉得讲得通俗易懂的首推EOPL，中文资料[这里](https://www.zybuluo.com/darwin-yuan/note/424724)有个介绍写得不错。

EOPL里面的讲得好理解，但内容有点浅。它是一个最基础的版本，并没有写如何实现structural polychromy，也就是没有多态的record和variant类型。我觉得variant是一个比较重要的类型，但是似乎有点复杂，一直都没想明白。

[之前的想法](type-system.md)是简化问题。其实是想绕开，所以当时的一个设想是，如果不能为类型命名，就不会产生类型递归；record干掉field名字，就可以简化成tuple；只用record带个数字的tag，就可以模拟variant的效果。其实直觉的方向还是对的，只是对structural polychromy的理解还不够。

对于一个东西，如果想不明白它是怎么实现的，我就不能说我懂了。最近突然有点感触：variant的类型应该是它本身。

王垠在《到底是谁在欺负我们读书少？》里面提到过，"每一个函数的类型，就是这个函数本身。没有比函数本身更能够描述函数的特征的类型"。现在突然想起来，醍醐灌顶。不单是函数，一些表达式的类型，都应该是它本身。

假设有个函数`(define id (lambda (x) x))`，问它的类型是啥？这个简单的例子，大家都知道是`a -> a`，那么如果调用过`(id 5)`之后，在这个上下文里面，它的类型是啥？变成了`int -> int`，因为类型参数实例化了。王垠指责HM系统里面let-polymorphism的问题，就是说的实例化的时机问题。`(id 3) (id "string")`，在同一个上下文里面，不能将id同时实例化成`int -> int`和`string -> string`。let-polymorphism作为HM系统里面的一条重要规则，在这个地方做了一些workaround，被王垠喷成了"根本性的错误"。扯远了，那么id的类型是什么呢？它其实即是`a -> a`，又是`int -> int`，又或者是`string -> string`，取决于它所处的上下文。因为它所在的上下文，决定了类型参数的实例化的时机。有了上下文我们才知道它是什么，没有什么比函数本身更能够描述函数的特征的类型。

Curry-Howard同构，对于类型系统这是一个重要的东西。对于id来说，把lambda换成forall，可以改写成这样：`(type id (forall (x) x))`，是不是跟`(define id (lambda (x) x))`长的一个样？做一个alpha变换x改成a后变成了`(type id (forall (a) a))`，也就是`a -> a`。函数表达式里面的变量，跟类型系统里面的类型变量，关系是对应的。多态类型之所以不好理解是因为它比简单类型多了一个维度。

为了支持record和variant，先引入以下表示：

    (type list (a) (Var (:cons a (list a)) (:nil)))
    (type fruit (Var (:apple string) (:orange)))
    (type r1 (Rec (:name string) (:value int)))
    (type r2 (Rec (:name string) (:value float)))
    
Rec表示是一个record，Var表示是一个variant。也就是在原来基础HM类型上面添加了Rec和Var两种类型。其实写法是几乎一样的，只不过用的是lisp，换成其它语言：

      datatype 'a list
      | Cons of 'a
      | Nil
      type record = {name: string; value: int}

上面定义了r1和r2，这俩不是一个东西。函数r1.name用于取出r1里面name字段，函数叫r2.name用于取出r2里面name字段，它们的类型分别是：

    r1.name : r1 -> string
    r2.name : r2 -> string

假如我又想定义一个函数，它可以从任何的record里面，取出name字段，如何描述这个函数的类型？

    get-name-field : (Rec (:name string)) -> string

子类型是一个比较复杂的问题，觉得往那个方向走下去就走偏了。为了不走偏，我们不能把这个类型具体化的定义出来。

再看看fruit的定义`(type fruit (Var (:apple string) (:orange)))`，这里的`:apple`就是ML里面的Constructor。它的类型是啥？是一个`string -> ?`，而`?`又是啥？是一个apple，apple是fruit这个variant其中的一种。怎么描述，都没有它本身在所处的上下文准确。因为如果还有其它的variant里面也定义了`:apple`，就歧义了。

看看case语句

    (case x
      (:apple s) -> e1
      (:orange) -> e2)

s的类型是什么？整个case表达式的类型又如何描述？当我们实现类型推导的时候，要把类型给写出来，但是这个类型确实不太好做形式化描述。

然而它的类型是什么其实可以不用关心。不需要类型，只需要约束。用约束描述比类型更准确。我们可以生成这样的约束:

    (== (typeof e1) (typeof e1))
    (== (typeof s) T1)
    (== (Var (:apple string)) T1)
    (<= (Var (:apple string)) fruit)

e1和e2的类型必须相同，假设s的类型叫T1，那么T1是一个类似`(Var (:apple string))`的东西，并且还是fruit是的一种。x是一个fruit类型，最终的结果跟e1和e2的类型相同。

弄了个非常粗糙的[demo代码](https://github.com/tiancaiamao/yasfs/blob/f74815cdd7e04edc6834a2f33469e41b0c08a06b/infer.scm)验证一下想法。
