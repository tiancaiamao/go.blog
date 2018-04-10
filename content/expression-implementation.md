表达式计算在一些地方会用到，比如 SQL 语句里面，`select * from t where a + 3 > b`，其中的 `a + 3 > b` 就是一个表达式。如何将表达式尽可能实现的高效呢？

## AST 解释器

表达式解析出来，会变成一个 AST 树。遍历这个 AST 树进行解释，可以计算出表达式的值。

    func eval(e Expr) {
        switch e {
        case AddExpr:
            return eval(e.left) + eval(e.right)
        case GTExpr:
            return eval(e.left) > eval(e.right)
        case ConstExpr:
            return e
        }
    }
    
这种基于 AST 的树型解释器不太高效。原因是解释执行嘛，多了太多额外的操作。每遇到一个 Expr，都要判断它是哪种类型；对 AST 树遍历的过程；eval 函数调用这些开销。

还要面临的一个问题是值的类型，加法的输入输出都是整型，而大于的输出是布尔型。可能就要用一种统一的格式来表示所有类型，比如

    type Value {
        Kind
        I int
        B bool
    }
    
这种打包的通用类型在计算和分配的开销也会比原生类型大得多。

## 更快的解释

在前面的实现方式里面，我们不停在查看一个表达式是什么表达式，再决定调用什么计算方法。这相当于把 AST 当数据在看待。而如果能把 AST 当计算看待，整个 AST 是一个 executable 的东西，就可以降低 “查看是什么数据，再做什么操作” 这种访问模式的开销。

具体做法，我们可以把表达式看成一个实现了 Eval 接口的东西。只要调用 `expr.Eval()`，就可以得到计算的结果了。

    type Eval interface {
        EvalInt() int64
        EvalBool() bool
    }
    type ConstExpr struct {
        v int64
    }
    func (e ConstExpr) EvalInt() int64 {
        return e.v
    }
    func (e AddExpr) EvalInt() int64 {
        a := e.left.EvalInt()
        b := e.right.EvalInt()
        return a + b
    }
    func (e GTExpr) EvalBool() bool {
        a := e.left.EvalInt()
        b := e.right.EvalInt()
        return a > b
    }
    
这里用了 EvalInt 和 EvalBool 是处理通用类型问题。注意跟之前的区别，表达式不是当数据看待的，而是当作计算看待。只要把表达式构造出来了，它就可以 Eval 了。

但是这种方式性能仍然不是很好，为什么呢？因为虚函数调用的开销太大了。比如执行一个加法表达式，调用虚函数对左右孩子计算 `e.left.EvalInt()` 和 `e.right.EvalInt()` 的过程，远大于 `a + b` 加法运算本身。

## 字节码和栈计算器

字节码可以把 Expr 的计算操作 “打平”，变成一些指令。最常见的，基于栈的计算器：计算左孩子，将左孩子的计算结果进栈；计算右孩子，将右孩子的计算结果进栈；将栈顶的两个数据进行操作，结果放回栈里面。

      for pc := 0; pc < len(bc); pc++ {
          switch bc[pc] {
          case Add:
              stk[top] = stk[top] + stk[top-1]
          case GT:
              stk[top] = stk[top] > stk[top-1]
          case Const:
              stk[top] = v
          }
      }
      
我测试了一下字节码模式，发现比上面一种实现略快一些。对比 `3 + 5 < 42`，执行 10 万遍：

    使用执行基于 AST 的快速解释：2.153914ms
    使用字节码方式：1.145872ms
    
那么字节码解释器的开销在哪里呢？进栈和出栈太多的数据交换是比较低效的。尤其是这里要走内存，而不能用到寄存器。

寄存器操作速度在 1ns 数量级的，而内存不带cache至少慢100倍。我们看一个简单的例子：

	var x int
	for i := 0; i < 1000000; i++ {
		x++
	}
    x是全局变量时：1.585067ms
    x是局部变量时：301.885µs
    
因为全局变量没法优化，必须从内存读取和写回内存。而局部变量直接放寄存器里面了。

还有字节码的指令分发，浪费的指令也高于操作本身。switch case 以及取字节码和修改 pc 这些。

## CodeGen 与 JIT

如果把上面的字节码，用原生指令实现，性能上就可以有数量级的提升。直接执行 Go 代码的 `3 + 5 < 42` 10万次要用多久？只需要 28.78µs。

正统 JIT 的做法，应该是拿 AST，去生成相应的操作指令，可以利用一些三方库比如 LLVM。但是 Go 语言调用 LLVM 太蛋疼了，需要走 cgo，用 Go 调的开销太大。用 LLVM 做 JIT 的另一个缺点是代码调试的友好性，中间生成的 IR 有 bug 对调试之类的是不太方便的。

其实最理想的形式，是宿主语言实现了 eval，[调用 eval 就可以得到原生性能，就像我之前说的](eval-as-universal-machine.md)。可惜这只是个奢求。

另一个方法是做 CodeGen。我们把 AST 往反方向做，用它生成宿主语言的代码，再去编译到原生，然后用某种方式 load。

最后在这里放一个好玩的例子，抛专引玉。

    const code = `package main
    import "fmt"
    func F() {
    fmt.Println("hello world")
    }`

    func main() {
        ioutil.WriteFile("/tmp/test.go", []byte(code), 0644)
        cmd := exec.Command("go", "build", "-buildmode=plugin", "-o", "/tmp/a.so", "/tmp/test.go")
        cmd.Run()

        p, _ := plugin.Open("/tmp/a.so")
        sym, _ := p.Lookup("F")
        sym.(func())()
    }
