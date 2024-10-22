如果分为上下两篇来写，应该是上篇先介绍 HM 类型系统，以及在 HM 下的 type checker 的实现。
鉴于这一块网上能够找到非常多的资料，可谓是汗牛充栋，所以上篇就直接略过了，进入到下篇。

按照网上的常规教程，一个 HM 的 type checker 大概会实现成这样子：

```
(func check-type
      x t env s => (unify 'int t s) where (number? x)
      x t env s => (unify 'bool t s) where (boolean? x)
      ...
      x t env s => (let find (assq x env)
			(if (null? find)
			    (error "variable not bound")
			    (unify (cdr find) t s))) where (symbol? x)
      ['if e1 e2 e3] t env s => (let s1 (check-type e1 'bool env s)
				     (let _ (check-type e2 t env s1)
					  (check-type e3 t env s1)))
      ['/. x e] [a '-> b] env s => (check-type e b (extend env x a) s)
      [f g] b env s => (let a (tvar)
			    (let s1 (check-type g a env s)
				 (check-type f [a '-> b] env s1))))
```

check-type 是 type checker 的主函数，它的第一个参数是 expr，第二个参数是类型描述，第三个是 env 第四个是 subst。第三第四个参数先跳过，这个函数做的事情就是检查 expr 的类型为是否为 t。

对于第二个参数类型的表示，这里的约定是这样的：

- 最简单的 'int 'string 'bool 'symbol 这种常量类型表示
- 然后是函数类型，用 -> 来表示，比如 '(int -> bool) 是一个函数，接受 int 类型返回 bool 类型
- (tvar) 会生成一个类型变量
- 多态类型。没有提供 (forall (a b) (a -> b))，它等价的表示是 (let a (tvar) b (tvar) [a '-> b])

像 int / string 以及 -> 没涉及到类型变量的时候，它是 monotype，而涉及到 (forall (a) ...) 这种它是 typeschema。
这里没提供 forall 表示而是直接写成 a -> b，是为了方便实现。概念上如果 a 和 b 是类型变量，则 a -> b 都等价于 (forall (a b) (a -> b))。

简单的解读几条上面的规则：如果要证明 [if e1 e2 e3] 的类型为 t，则需要证明 e1 的类型是 bool，并且 e2 和 e3 类型都是 t。

如何证明 ['/. x e] 的类型是 [a -> b] 呢? 在只涉及到 monotype，不涉及 typeschema，也就是没有引入 forall 和类型变量的时候，对这条规则的解读是 x 的类型为 a，在这个假设下继续证明 e 的类型是 b：


      ['/. x e] [a '-> b] env s => (check-type e b (extend env x a) s)

当存在类型变量后，如果 a 和 b 是类型变量，这条规则的写法还是这样子，但是理解需要更新成另一种解读方式: (a -> b) 实际上是 (forall (a b) (a -> b)) 的缩写形式，因此实际上检查的是表达式 ['/. x e] 类型为 (forall (a b) [a '-> b])。要证明 ['/. x e] 是对于任意的类型变量 a b，它的类型为的类型 [a '-> b]，我们假设以 a1 来实例化类型变量 a，以 b1 来实例化类型变量 b，则在 x 类型为 a1 的情况下，e 的类型为 b1。**这里有一个实例化和替换的过程，不再是 x 类型为 a，然后 e 的类型就是 b。而是说，以任意一个具体的类型 a1 来实例化类型变量 a，以任意一个具体类型 b1 来实例化类型变量 b**。a1 可以是 int/bool/string/->/或者任何高阶的类型。我们最后 check 的实际上是 ['/. x e] 是 [a1 -> b1]，而 [a1 -> b1] 是 (forall (a b) (a -> )) 的任意一个实例化。

这条规则本可以写成

      ['/. x e] [a '-> b] env s => (let a1 (tvar) b1 (tvar)
                                    (unify a1 a) (unify b1 b)
                                    (check-type e b1 (extend env x a1) s))

只是我们每一个 (tvar) 都是唯一的，因此可以直接缩写。[a '-> b] 就是 (forall (a b) (a -> b))，也就是 (let a (tvar) b (tvar) (a -> b))。

如果用 beta 替换的角度看这条规则，整个这个过程就是将表达式 e 中出现的全部的变量 x 替换为类型为 a 的一个新的变量 c，得到表达式为 (beta e x/c)，替换后的表示式 (beta e x/c) 的类型为 b。

```
    ['/. x e] : [a '-> b]
------------------------------------
    c : a,  (beta e x/c) : b
```


如何实现其它类型的检查呢，比如 (list any)? 这里就需要扩展上面的 type check 的算法了，添加 list 的检查规则:

```
;; 空的 list 是 (list any) 类型
['list] ['list a] env s => s 

;; 如果要证明 (cons x y) 是 (list a) 类型，则需要证明 x 的类型是 a 并且 y 是(list a)
['cons x y] ['list a] env s => (let s1 (check-type x a env s)
		 (let s2 (unify t ['list a] s1)
		  (check-type y ['list a] env s2)))
```

那么继续加 tuple / maybe / either 等等类型呢？规则会继续追加。这里有一个问题是，这些都是系统内置的类型，都是实现在 check-type 函数里面了。

如果我们需要加自定义的类型怎么办? 比如说我想定义 datatype (fruit) = apple | orange | banana？ 我们需要有某种手段把类型检查的规则变成可扩展的才行。这就是本文"一个强大灵活且巧妙的 type checker 实现思路"最核心的点了：

```
(set '*type-rule* ())

(defun check-type (exp typ env subst)
 (let handler (if (pair? typ)	
	       (let find (assq (car typ) *type-rule*)
		(if (null? find)
		 check-type-core
		 (cdr find)))
	       check-type-core)
  (handler exp typ env subst)))
```

自定义类型表示全部是带括号的表示，比如 (fruit) 类型。我们把自定义类型的检查规则暴露出去，从 `*type-rule*` 中获取相应的 check-type 的函数。然后类型检查就非常灵活了。定义一个新的类型就是实现这个类型的类型检查函数，函数里面类型检查规则用户想怎么写就怎么写。这是我们的 (fruit) 类型：


```
(func type-check-for-fruit
      x ['fruit] env s => s where (elem? x ['apple 'orange 'banana]))
(set '*type-rule* (cons (cons 'fruit type-check-for-fruit) *type-rule*))
```

fruit 的例子是一个 union，换成 my-struct 之类的的也非常简单：

```
;; (func type-check-for-struct
;;       [a b] ['my-struct 'int 'bool] env s => (let s1 (check-type a 'int env s)
;;       						(check-type b 'bool env s)))
```

list 这种是带了变量类型的例子。list 不也需要实现到默认规则里面，可以改成自定义的，然后加入到 `*type-rule*`。

```
(func type-check-for-list
 [] ['list a] env s => s
 ['cons x y] ['list a] env s => (let s1 (check-type x a env s)
		 (let s2 (unify t ['list a] s1)
		  (check-type y ['list a] env s2))))
(set '*type-rule* (cons (cons 'list type-check-for-list) *type-rule*))
```

类似地我们可以添加 tuple 的定义:

```
(func type-check-for-tuple
  ['cons x y] ['tuple a b] env s => (let s1 (check-type x a env s)
  						(check-type y b env s1)))
(set '*type-rule* (cons (cons 'tuple type-check-for-tuple) *type-rule*))
```

注意，我们在 list 和 tuple 的规则里面，都写了一条 ['cons x y]，是不是冲突了？不是，这样写是可以的。主打一个灵活和随心所欲。这么写之后，我们

(check-type ['cons x y] (list a) ...) 可以成功，而 (check-type ['cons x y] (tuple a b) ...) 也可以成功，也就是说 (cons x y) 这个 expression 既可以用于表示一个 list，也可以用于表示一个 tuple，取决到我们在做 type check 的时候希望它是什么。代价嘛，很明显，我们只能 type check 而不是 type infer，在遇到一个 (cons x y) 之后我们无法 infer 它的类型到底是 list 还是 tuple。

按传统的方式把 (maybe) 实现到默认类型检查中，它会是 (Either t) | Nothing。但是我们可以不必那样，可以这么做：

```
(func type-check-for-maybe
  () ['maybe t] env s => s
  exp ['maybe t] env s => (check-type exp t env s))
(set '*type-rule* (cons (cons 'maybe type-check-for-maybe) *type-rule*))
```

() 可以被解释成空 list，甚至也可以被解释为 maybe 类型中的 nothing 部分！这就是灵活性的表现。不要拘泥于其它语言怎么干的。


然后再看看 monad。所有实现了 bind + return 规则的类型都可以被视为一个 monad:

```
(func type-check-for-monad
 [return x] ['monad a] env s => s
 [bind ma f] ['monad a] env s => (let s1 (check-type ma ['monad a] env s)
	                                (check-type f (let b (tvar) [a '-> ['monad b]]) env s1)))
(set '*type-rule* (cons (cons 'monad type-check-for-monad) *type-rule*))
```

注意，这里用的是 [return x] 和 [bind ma f] 而不是 ['return x] 和 ['bind ma f]，并且 return 和 bind 在 pattern match 中是没被使用到的，所以它实际相当于:

```
 [_ x] ['monad a] env s => s
 [_ ma f] ['monad a] env s => (let s1 (check-type ma ['monad a] env s)
	                            (check-type f (let b (tvar) [a '-> ['monad b]]) env s1)))
```

也就是说我们只检查了部分的类型，没有完整地检查 bind 和 return 的实现是否复合要求。这里举例是故意这么做的，**我们可以不检查全部的约束，只检查部分约束**。检查规则是由用户自己实现的，用户决定要检查哪些部分。还是主打一个灵活。那代价是什么呢？**如果用户实现了这样的规则，则通过了类型检查，不保证代码是正确的。而无法通过类型检查，则代码一定是有问题的**。故意让类型检查规则可以不完备，这样选择也符合 type check 的语义。

动态类型的语言灵活表达能力强，但是不安全。如果加上类型系统之后，安全但表达能力取决于类型系统的复杂程度。想要既简单，又安全，又灵活...那就是不可能三角。安全这玩意嘛，就像 fuck 的时候要带上套套，所以是这个不可能三角里面可以牺牲一下的。暴露类型检查函数可以让用户自己写规则，自己决定检查的时候是否要安全。

关于子类型，HM 系统的灵魂是 unify，它是等价关系而不是 <= 关系，所以扩展的时候基础的 HM 是不好支持子类型的。但是注意到我们用的是 type check 而不是 type infer，也就是默认带了标注，这就有很好的优势来支持子类型。比如说我们去对(cons 1 (cons 2 ())) 调用 check-type，它的精确类型是 (list int)。(list int) 是 (list any) 的子类型，所以如果我们这样写，检查仍然是可以通过的:

```
(check-type '(cons 1 (cons ())) (let a (tvar) ['list a]) ..)
```

写代码的时候要求全部的函数需要写上函数签名，然后对于非函数定义的全局的表达式，我们可以不写类型签名。在做 type check 的时候，如果遇上了不带签名的表达式，我们直接 check-type exp any 就好了，假设它的类型是 any，这时的 type check 就相当于在做 type infer。在 HM 系统里面，any 只可以被实例化成 monotype，所以如果一个表达式的返回值是 typeschema 的时候会失败。这种情况下还是需要加上标准。幸运的是对应的场景应该不会太多，所以标注的负担是可接受的。

再看一个例子是 signature 的实现，或者说类似于 Go 里面的 interface 那种。所有实现了 ToString() 方法的都是一个 Stringer 的 interface。所以我们定义 Stringer 类型，就是实现 Stringer 的类型检查函数：

```
(func check-type-for-stringer
    exp (Stringer) env s => ;; 如何检查是否 exp 实现了 ToString 方法呢?
```

在 Go 里面的实现是需要依赖于 runtime check。我们的 type checker 不是发生成 runtime check 阶段的,所以需要修改一下实现形式，如果一个类型实现了某个 interface，则需要显式地声明。有了这种关系之后，比如 (impls (mytype1) (Stringer)), (impls (mytype2) (Stringer)) 上面的 check 就可以做成


```
(func check-type-for-stringer
    exp (Stringer) env s => (or (check-type exp (mytype1) env s)  (check-type exp (mytype2) env s) ...))
```


最后的一个例子是 set 类型。

```
(func check-type-for-set
    [] (set t) env s => s
    ['insert a b] (set t) env s => (begin (check-type a t env s)
                                        (check-type b (set t) env s)
                                ;; 还需要检查 a 在 b 中是唯一的)

(func insert
    a b => (if (element? a b)
                b
                (cons a b)))
```

在 type checker 里面无法做的 runtime 检查，但是我们可以假设它是成立的。这就像 gradual typing system 里面，能静态检查的部分做静态检查，而需要动态检查的部分做代码插入。这里的 set 是假设 runtime check 会保证 `(element? a b)`，于是只要通过 ['insert a b] 之后的类型一定还是一个 set。


ok，通过这么多例子应该是讲得很清楚了。整个 idea 其实是 inspired by shen 语言的。在 shen 语言里面，它的类型系统就是通过 "宏 + 逻辑引擎"来实现的。定义类型就是写相继式演算的形式，描述类型的规则。不过 shen 语言要求使用都懂逻辑，这就有一点"自函子范畴上的一个幺半群"的味道。程序员哪玩得明白这么复杂的东西。

我将这个思路改成了 "定义类型就是实现这个类型的类型检查函数"，在 HM 的基础上扩展一点点，只要能照着网上的 demo 写一个 HM 的类型检查，就能够搞清楚整个 type checker 如何 work 的。这个门槛是低于 shen 语言的，灵活性却等价于它。
