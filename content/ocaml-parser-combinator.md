体验了一把ocaml。好像是看知乎，推荐一门实用的函数式语言提到ocaml。haskell由于是"纯的"函数式，实用性比较差，而ocaml是允许命令式的，既可以体验函数式的精髓，又可以用来实际写东西。很多概念不用特定的语言是很难以表达出来，像类型系统，monad这些就必须要ocaml或haskell。像continuation，cps就最好是scheme。

ocaml还挺合味口的。跨平台，既可以bytecode的形式虚拟机执行，又可以编译成原生代码。runtime非常轻量，基本上只有一个GC。在GC算法方面，做了分代和增量，所以延迟时间理论上应该可接受。

光看语法是学不会一门语言的，所以找了一个非常最适合函数式语言的场景练手：parser combinator。会涉及到高阶函数和monad，monad是个挺难理解的东西，以前折腾scheme时接触过一点，这次理解应该更深了。

虽然支持闭包的语言，都能够写parser combinator。但不同的语法，有的表达出来更自然，而有的就很别扭。之前其实用Go语言尝试写过，主要两个地方需要注意：一个是如何处理递归定义的语法，另一个是对生成的ast结构重新组织。等下会看在ocaml语言中如何解决这两个问题。

先定义一个parser的类型：接受一个char的序列，如果能解析，返回解析和结果和剩余的char；如果不能解析则返回None。
    
    type 'a parser_t = char list -> ('a * char list) option

定义几个简单的辅助函数：

    let satisfy pred = function 
      | [] -> None 
      | x::xs -> if pred x then Some(x,xs) else None;;
    let range a b = satisfy (fun x -> x>=a && x <=b);;
    let exactly x = satisfy ((=) x);;

satisfy函数接受一个谓词，返回一个parser。接着定义一些基本的parser：

    let digit = range '0' '9';;
    let lower = range 'a' 'z';;
    let upper = range 'A' 'Z';;
    let letter = lower <|> upper;;

parser combinator，顾名思义，就是把一些最基本的parser像搭积木一样组合(combinator)起来，发挥无穷威力。我们看上面的`<|>`，就是一个parser combinator，它接受两个parser作为参数，lower可以parse小写字母，upper可以parse大写字母，返回一个新的parser可以解析大小或小写字母。`<|>`是这样子定义的，先尝试用第一个parser去解析，成功就返回结果，不成功再用第二个parser去解析：

    let (<|>) p q = fun x -> match p x with Some _ as res -> res | None -> q x;;

“或”比较容易实现，是因为不涉及到返回结果的重新组织，返回的ast必然是两之中的一个。当我们想接着实现一个AND的时候，就会遇到问题：结果会是两者的组合，这是一个难点。比如说我们有

    sequence parse1 parse2 parse3 parse4 ...
    
如果不对返回值处理，ast的结构不会是预想中的

    [ast1; ast2; ast3; ast4 ...]
    
而是类似

    [ast1; [ast2; [ast3; [ast4 ...]]]]

或者

    [[[[ast1] ast2] ast3] ast4...]
    
需要重新整理`char list -> Some(ast, char list)`里面的ast，方法就是用monad。这里的monad其实就是parser，隐藏在里面的状态是ast。为这个monad实现对应的bind和return函数：

    let (>>=) m f = fun l -> match m l with
      | None -> None
      | Some(res, l1) -> f res l1;;
    let return x = fun l -> Some(x, l);;

这里就不具体讲monad了，因为monad这东西，自己能理解的时候突然就懂了，而不懂的时候别人怎么讲都讲不懂。没办法，有智商门槛。

有了monad我们可以做一些比较酷的事情，比如接下来定义的几个函数：

    let (=>) p q = p >>= fun r -> return (q r);;
    let (>>) p q = p >>= fun _ -> q;;
    let (<~>) x xs = x >>= fun r -> xs >>= fun rs -> return (r :: rs);;

`(=>)`的功能是用函数q改写解析结果，这样子读：先用parser p处理输入，再将得到的结果用q进行处理。

`(>>)`的功能是忽略p解析出来的结果，这样子读：先用parser p处理输入，丢弃结果，继续用q处理剩下的输入。

`(<~>)`是用来将结果联结起来的函数，参数xs是一组的parser，将x处理结果和xs处理的结果联结作为最终结果。

剩下一些其它的combinator定义，比如下面这些，都不难实现的。

    optional p;; 可以parse p或者无动作
    many p;; 重复用p做parse，零或多次
    many1 p;; 重复使用p解析一次或多次
    sequence p1 p2 p3 ...;; 顺序的使用p1 p2 p3
    
基本上parser combinator就是这些了。

最震撼的时刻！看看ocmal中用parser combinator写一个能解析`if xxx then { xxx } else { yyy }`语句的parser大概是长什么样子：

    if_stmt input =
      (token "if"   >> (* if *)
      expr         >>= fun pred ->
      token "then" >> (* then *)
      token "{"    >> (* { *)
      stmts        >>= fun thn ->
      token "}"    >> (* } *)
      token "else" >> (* else *)
      token "{"    >> (* { *)
      stmts        >>= fun els ->
      token "}"    >>
      return (IfElse (pred, thn, els))) input

--------------

可以看到，ocaml这样的函数式语言，表达能力上面还是非常强大的，一个基本的C的parser估计也就400~500行的样子能搞定。不过写代码时死掉的脑细胞就不能用代码行数来衡量了，理解monad就是一个坎。

想了解下函数式语言的，推荐ocaml语言。入门书的话推荐[《Developing applications with Objective Caml》](http://caml.inria.fr/pub/docs/oreilly-book/html/index.html)。

说到ocaml，看起来跟rust语法有那么一点相似的感觉，rust早期的编译器也是用ocaml写的。不过语法是非常表面的东西，两者完全不一样，rust根本不是一门函数式语言，大概类型推导是唯一从ocaml那里借鉴到一点有价值的东西。

哦，漏了解释前面的两个问题。如何处理递归定义的语法？

    UnaryExpr = '(' Expr ')'
    Expr      = UnaryExpr | BinaryExpr
    
先定义UnaryExpr的parser再定义Expr的parser，或者先定义Expr的parser再定义UnaryExpr的parser，都会出现定义后者时前者还未定义的问题。在其它语言比如Go中可能通过引用类型来解决。在ocaml中，允许相互递归定义，可以写在一个let and语句里面，用`let UnaryExpr = xxx and Expr = xxx`解决。

至于如何对生成的ast结构重新组织？monad！上面的例子就是。

好吧，又尝试了一门新语言。以后自我介绍时，加一句：精通各种语言写hello world。

最后，[代码的gist](https://gist.github.com/tiancaiamao/6f2d274e2be223a831fd953e72836e3c)这里可以看到。
