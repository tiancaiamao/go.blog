上周研究了一下SQL查询是如何实现的。结论是，跟做编译器没什么本质的区别。anyway，SQL也是一门语言嘛，这是一个叫query compiler的话题。

从大的层面上讲，对于一门通用语言的编译，大概要经历词法分析语法分析，得到抽象语法树(AST)，然后转化为内部表示(IR)，接着是真正的编译步骤，最后会做代码生成。一直到生成AST都不用对比，对于SQL的编译，到了后面会有点区别，它要经历的步骤是逻辑计划和物理计划。

逻辑计划会将查询转化为关系代数，关系代数相当于内部表示(IR)。然后执行关系代数的等价转换，这个步骤重写是逻辑优化。涉及到的包括将选择下移，子查询的消除等。还需要进行代价估算，选择出最优的逻辑计划。

逻辑计划生成之后，就是一堆的关系代数：投影，选择，连接，笛卡尔集，并，交，差等等运算。但是在这一层抽象中，逻辑计划并不知道怎么执行。物理计划则是将逻辑计划生成一些更基础的运算，比如TableScan，IndexScan。比如select语句编译到物理计划之后可能就是Filter+IndexScan。

当然还会有许多的细节，每一个大的阶段都会有许多小的步骤，而很多小步骤中可能都涉及到比较深入的算法。

SQL的实现其实就是把SQL语句编译成一些很基础的运算，然后在一个runtime之上执行这些运算。

脑洞大开想到一个事情是，逻辑计划用lisp语法表示。因为生成关系代数之后它就是一颗树型结构，而树型结构跟lisp的语法是完全等价的。
  
我们看这个SQL语句：

    select distinct s.SName
    from Student s, Attend a, Lecture l, Professor p
    where s.SNo = a.ASNo and a.ALno = l.Lno
    and l.LPNo = p.PNo and p.PName = 'Larson'
  
如果转化为关系代数之后，用lisp语法表示的，它就是这样子

    (Projection
      (Selection
        (AND (= s.Sno a.ASNo)
            (= a.ALNo l.LNo)
            (= l.LPno p.Pno)
            (= p.PName 'Larson'))
        (Product
          (Product
            (Product
              (Projection Student s)
              (Projection Attend a))
              (Projection Lecture l))
          (Projection Professon p)))
    SName)

规划重写比如关于选择和投影的：

    (Selection (AND C1 C2) R) == (Selection C2 (Selection C1 R))
    (Projection (Selection C R) col) == (Selection C (Projection R col))

那么逻辑优化的实现其实就是一个lisp的macro，将输入按一定的规则进行重写生成另一个形式。如果是lisp表示，可以专注于这一步的算法，好处是：不需要关心parser的东西；输入和输出都是可视化的(lisp grammar)。
