研究 [ribbit](https://github.com/udem-dlteam/ribbit/) 的时候无意中得到的启发。ribbit 仅用很短的代码，实现了尾递归和 first-class continuation，它是编译到一个字节码的虚拟机指令，虚拟机设计得很简化，因此很容易在不同的宿主语言中实现，比如 scheme, C, Go, javascript 等。

代码生成那一块思想，其实有这篇论文 [Using closures for code generation](http://www.iro.umontreal.ca/~feeley/papers/FeeleyLapalmeCL87.pdf)的影子。这是 87 年的一篇论文，相当的老了。不过 Marc Feeley (gambit scheme 作者) 出品真的都是好东西啊！

早在《lisp in small pieces》的书里面，当时读到有一章，我其实应该已经接触到这个思想了，它是把每个虚拟机指令用一个宿主语言的闭包来实现的。虚拟机指令粒度比较细，用闭包做会有一些性能损失，直接用一个大的 switch 或者 threaded code 分发性能可能更高一些。
由于当时思考的角度不对，而且对 lisp 的理解深度还不够，站在当时的视角，并没有体会到其中的精妙处，所以当时并没有开悟。

直到今天，终于悟到了，“闭包即代码生成”。

插入说一句，在 [shen-go 1.0rc 发布](shen-go-v1.0.md)的时候，悟的是 "编程语言即虚拟机"。如果我把一门编程语言当虚拟机用，把 lisp 生成到那门语言，基本可以做到一种不损失性能的方式。没有字节码虚拟机之类的解释开销。如果宿主语言能编译到 native，那么就可以让 lisp 间接生成到 native。

这样性能是好了，但是损失了动态编译的能力。换另一种方法，AST 树解释器的方式，实现比较简单，可以动态解释，但是性能非常差。
用字节码解释器的方式，也有动态解释能力，性能居中，但是代码复杂度增加了一些。

所以，有没有一种，实现起来又简单，代码复杂度不高，性能又过得去的方式呢？这就是今天领悟到 -- 闭包即代码生成。

代码生成要生成一个可执行的运行，要么是生成汇编指令，要么生成虚拟机的字节码指令，要么生成到某个语言的源文件...
这些都需要一次额外的编译步骤，才能变成一个可执行的东西。怎么生成的一个直接可执行的东西呢？闭包！闭包是可以直接被执行的。

代码生成就是生成闭包。只需要把解释器改一改，把解释的过程，变成生成闭包的过程。
使用用宿主语言里面的闭包，去实现目标操作的逻辑即可。生成出来的闭包，就是直接可运行的。

以 Go 作为宿主语言的例子，编译常量，可以生成这样的闭包：


```
func genConstInst(c Obj) {
    return func (e *Env) Obj {
		return c
	}
}
```

编译变量，生成这样的闭包：

```
func genVariableInst(c Obj, env Obj) {
	// 编译期确定好变量的环境的 offset
    offset := findInEnv(c, env)

    return func (e *Env) Obj {
		// 执行期通过数组的 offset 来定位变量，不用查找过程
	    return e.Get(offset)
	}
}
```

编译 if 语句：

```
func compileIf(a, b, c Obj, env Obj) {
	// 分别生成好 a b c 的闭包
    a1 := compile(a, env)
	b1 := compile(b, env)
	c1 := compile(c, env)
	
	// 然后是生成 if 对应的代码(闭包)
	return func (e *Env) Obj {
		if a1(e) {
			return b1(e)
		} else {
			return c1(e)
		}
	}
}
```

生成 lambda 和生成函数调用的过程也是类似的，这里就不重复。只要会写 ast 的解释器，基本上就能把生成闭包的方式写出来。

写解释器的复杂度，实现了接近原生级别的性能(论文里面提到大概是宿主语言2-3倍以内)，妙哉！

还有一个更妙的想法是，如果我们宿主语言用 rust 实现，那么可以得到一个，不用自己实现垃圾回收，但是又带了垃圾回收功能的语言。
只是可惜我不想写 rust...

## 尾递归优化

在不支持尾递归优化的语言里面，"闭包即代码生成"的玩法需要做一些额外的处理。下面的函数，它编译成闭包代码之后，

```
(defun f (n)
	(if (= n 0)
		1
		(* n (f (- n 1)))))
```

每一步都会新增加一层的栈

```
generated_f
generated_if
generated_f
generated_if
generated_f
...
```

不做特殊处理会很容易导致爆栈，处理方法可以用 [trampoline](https://en.wikipedia.org/wiki/Trampoline_(computing)) 搞。

ribbit 那边它是把解释器做成了一个 CPS 形式，把 continuation 暴露出去的。做过 CPS 之后所有函数调用都不再返回，全部变成了尾递归形式。
我自己实现时不想做 CPS 或者支持 continuation，所以需要用 trampoline 去处理尾递归。

## 优化环境和栈的表示

如果用 SECD machine 做内部表示，会涉及到环境和栈。如果不需要 `call/cc`，有一个可以优化的点是环境的表示。

在调用函数的时候会生成一个新的环境，这个环境保存的是当前的函数调用的参数的变量绑定，环境对函数的 body 生效。


```
func genCall(f, args ...) {
    return func(e *Env) Obj {
		closure := f(e)
		
	    for _, v := range args {
			arg := v(e)
			frame = append(frame, arg)
		}

	    // 新的环境，这里会有分配
	    newEnv := &Env{
			parent: e,
			frame: frame,
		}
	    return closure.body(newEnv)
	}
}
```

环境是在堆上面分配的。如果语言中不涉及到闭包的时候，这里的环境不会被再次引用到，可以直接改成栈分配。

但是闭包肯定是有的，所以复杂一点的优化，就是把函数的本地变量，和闭包变量区分开来。
对于本地变量，只需要在栈上分配，而对于闭包变量，则需要堆上分配。


打算试试把 shen-go 的解释执行部分，用这种方式给重写掉。
