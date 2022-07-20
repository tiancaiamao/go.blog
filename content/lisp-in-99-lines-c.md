lisp 中一种巧妙的 GC 实现方式


我发现 [Lisp in 99 lines of C](https://github.com/Robert-van-Engelen/tinylisp) 和 [SectorLisp](https://justine.lol/sectorlisp2/) 里面都用到了这个 trick，挺有意思的，记录一下。

把对象只分为 atom(symbol) 和 cons 两类，cons 就是 stack 下移两个格子，存入 car 和 cdr。
随着不停地 cons，栈就不停地往下涨。

所有的东西，都有 cons 来表示。比如说环境就是 assoc list 的形式，闭包就是 lambda args + body + env 或者随便加个 closure 之类的 tag 的 list。

所有的构造对象的过程，就会造成栈往下涨。

eval 之前的 stack 位置记作 A。eval 之后的 stack 位置记作 B。A~B 就是执行过程中产生的数据，大部分可能是垃圾。我们只要把还要使用的部分对象，拷出来，比如这个时候 stack 的位置是 C，那么 B~C 这个区域的数据，拷到 A，就完成了 GC。SectorLisp 里面把这个叫 an ABC garbage collector。


99 lines 那个也是一个类似的思想，GC 就是把栈回到 eval 之前的位置。


