这几天优化[TiDB](https://github.com/pingcap/tidb)，我们的内存管理还不太理想，有大量的开销花在了GC上面，其中造成对象分配最多的，就是SQL的parser。所以今天想讲讲关于parser的故事。

TiDB中SQL的parser是利用的lex&yacc实现的。也就是走的parser generator的方式，这里可以看到[代码](https://github.com/pingcap/tidb/blob/master/parser/parser.y)。手写一个parser.y文件，然后用了一个叫goyacc的工具，它会根据我们定义的.y文件生成parser.go。但是对于做优化来说，生成的代码总是不那么容易控制。

一般来讲实现parser的主要形式有三类：纯手写parser，YACC工具，parser combinator。

纯手写parser的事情，我只在许多年以前，写自己的第一个scheme解释器的时候干过。还好lisp的语法足够简单，没有造成太大困扰。纯手写parser的主要问题是枯燥，工作量大。本来parser其实没有什么太高深的事情，简单的递归下降可以处理很多东西了。好处是性能可以做到比较极致，据说lua的parser早期是用工具做，后面考虑到性能就手写了，类似的例子还有一些。

如果想把性能优化做到极致，手写parser是一个方案。我搜索了一下Go的开源项目涉及到SQL的parser的实现，Cockroach跟我们一样用的yacc，想必会遇到我们同样的问题。[InfluxDB](https://github.com/influxdata/influxdb/blob/master/influxql/parser.go)是手写的。纯手写parser是这种样子的：

    // ParseStatement parses an InfluxQL string and returns a Statement AST object.
    func (p *Parser) ParseStatement() (Statement, error) {
        // Inspect the first token.
        tok, pos, lit := p.scanIgnoreWhitespace()
        switch tok {
        case SELECT:
            return p.parseSelectStatement(targetNotRequired)
        case DELETE:
            return p.parseDeleteStatement()
        case SHOW:
            return p.parseShowStatement()
        case CREATE:
            return p.parseCreateStatement()
        case DROP:
            return p.parseDropStatement()

先看哪些Statement，再具体到某一种，比如Select的Statement，再对应到解析它的格式的函数。

yacc是我们现在使用的方案。这种方案的缺点，yacc有一定的学习门槛；生成代码方案不那么可控，可读性基本为零；还有现在的问题，性能。哦，据说还有灵活性比较差，特定复杂的语法是处理不了的。

说下yacc的基本原理。在.y文件中其实是定义了一套语法规则，yacc工具会根据这套规则生成一张表。比如在生成的parser.go文件中，有一张名叫yyParseTab的表。生成的代码做解析时会根据这张表决定走到对应的解析规则。就白了就是状态机，模式匹配这些东西。

最后一类实现方式叫做parser combinator，是我比较喜欢的。如果自己从头实现，估计会采用这种方式。parser是一个接受输入，返回特定结果的函数。如果我们有许许多多不同的parser，每一类parser只处理一个非常简单的工作，这类parser是比较好写的。然后定义combinator，combinator接受parser作为输入，生成新的parser作为输出。利用高阶函数，combinator可能非常灵活而又功能强大。跟纯手写parser相比，它没有那么多工作量，而跟生成器相比，它是灵活并且可调试的。

最早知道parser combinator是从王垠那里，那么就拿他的代码为例子说下吧。仅用这几行代码就实现了一个[lisp的parser](https://github.com/yinwang0/ydiff/blob/master/parse-lisp.rkt)：

    (:: $open
        (@or (@~ "(") (@~ "[")))
    (:: $close
        (@or (@~ ")") (@~ "]")))
    (:: $non-parens
        (@and (@! $open) (@! $close)))
    (::= $parens 'sexp
        (@seq $open (@* $sexp) $close))
    (:: $sexp
        (@+ (@or $parens $non-parens)))
    (:: $program $sexp)

其中@or @and @+ 这些都是combinator，是高阶函数。`(@~ "(")`生成了一个只能解析`(`的parser函数，而用`@or`组合起来，得到的`$open`就是一个可以解析`(`或者`[`的parser。`@+`是将parser重复多次，比如多个sexp。

----------

回到最初的地方，parser的优化问题。给个十天半个月的，我大概可以写一个基本的parser出来，能解析80%的SQL语法。但是后面填坑就复杂了，还有跟现在的ast结构定义的兼容问题。TiDB已经beta版本了，干这种事情明显不值得。

我想了一个简单的优化方法：给yyParse传一个分配器进去。这个分配器非常简单，就是一块array。yyParse里面解析出ast对象直接从这块array里面划。每个session绑定一个parser对象，里面有分配器。因为每个session里面，上一个SQL执行完成之前，下一个SQL是不会执行的，所以不用担心会出问题。哈，太晚了，明天去改代码吧！
