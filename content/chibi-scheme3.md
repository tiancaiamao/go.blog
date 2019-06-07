## 代码流程

从sexp_eval_string函数可以看到整个代码的全部流程.

    sexp_eval_string = sexp_read_from_string + sexp_eval 

前者完成过语法分析，后者编译并执行 

    sexp_eval = sexp_compile + sexp_apply 

前面的compile完成编译生成字节码，后面apply在虚拟机上执行字节码。

    sexp_compile = sexp_analyze + generate 

编译的过程就是语法分析和代码生成的过程。

## 词法分析

词法分析主要是由 sexp.c 文件中的 `sexp_read` 函数完成。这个函数从外部数据流接收输入，读取一个 scheme 对象。lisp 特殊的语法(全是括号)使得这门语言的词法分析非常的简单。源代码中，`sexp_read`调用 `sexp_read_raw` 完成真正的工作。

`sexp_read_raw` 的大致结构如下：

    scan_loop:
    读一个字节
    若是EOF 返回SEXP_EOF
    若是换行或空格或\t，或\t或\r goto scan_loop
    若是' 返回(quote xxx)的链表
    若是" 调用sexp_read_string函数
    若是(
    　　递归调用sexp_read_raw读取一个原子
    　　while(不是SEXP_EOF,不是SEXP_CLOSE,不是SEXP_RAWDOT)
    　　　　用sexp_cons连成链表
    若是#
    　　再读一个字节
    　　如果是b或B 读一个2进制数
    　　如果是o或O 读一个8进制数
    　　如果是d或D 读一个10进制数
    　　如果是x或X 读一个16进制数
    　　如果是e或E 读一个符点数
    　　如果是f或F或t或T 读一个字符，返回SEXP_TRUE或SEXP_FALSE
    　　...
    若是; 读取注释放丢弃
    若是| 读取注释放丢弃
    ...
    若是0-9的数字 调用sexp_read_number
    否则  调用sexp_read_symbol

总体上就是一个循环，然后 switch-case 中分析前几个字符，它们判断类型后调用 `sexp_read_number`, `sexp_read_symbol`, `sexp_read_string` 等等或者递归地调用 `sexp_read_raw`。这个很容易，可以对照源代码看。

最初的外部输入，通过词法分析，变成了scheme的各种内部对象，如 flonum,string,char,vector 等，以及，最重要的 list 对象。这些进一步提供给语法分析进行处理。

## 环境

“环境”在 scheme 语言中是一个非常重要的概念。也许“上下文”是特定于 chibi-scheme 实现的重要概念，但“环境”绝对是任意 scheme/lisp 语言的编译器，解释器都共有的概念。

环境可以看成一个关联容器，存的是所有的符号-值的映射。每个符号，通过环境就可以找到它对应的 scheme 对象，也就是符号绑定到的值。

如果是解释器，环境是一直存在的。解释器每次遇到符号都要在环境中找其绑定的值，所以效率不高。而编译器，如 chibi-scheme 中， 编译阶段(准确地说是语法分析阶段)会借助于环境查找符号绑定的值，而生成字节码后，就可以直接使用值而不必查询绑定了。

环境在 chibi-scheme 中也是一个 sexp，其数据部分的结构为：

```
struct {  
  sexp parent, lambda, bindings;  
} env;  
```

其中 bindings 域就是`((symbol1 . value1) (symbol2 . value2) …(symboln . valuen))`这种结构。

各个 env 结构通过 parent 域连到一起。每当进入到一个新的 lambda 表达式(相当于其它语言中的函数)，就会生成一个新的环境，新环境的 parent 域指向 lambda 表达式定义所在的环境。

查找符号绑定时，如果能在当前 env 中的 bindings 中找到，则个这符号是局部变量。如果没有找到，则会沿 parent 链向上一级级地查直到找到。出现在 parent 链的 env 中的绑定叫做自由变量 (free value)。如果在 parent 链中也找不到符号绑定，则报错。

接下来分析整个标准环境的创建流程。对应的函数是eval.c文件中的 `sexp_make_standard_env_op`。

    sexp_make_standard_env_op = sexp_make_primitive_env + sexp_load_standard_env

后者主要会加载初始化文件init.scm。前者先执行sexp_make_null_env，再加载opcodes表。opcodes表在opcodes.c文件中，这个opcodes表包含了scheme语言的基本的函数的opcode，如car，cdr等的opcode结构。

`sexp_make_null_env_op` 调用 `sexp_make_env` 分配一个 env 结构，再初始化 `core_forms` 表。注意 `core_forms`和 opcode 都是 scheme 的基本符号绑定到的 scheme 对象。基本表（如 if,quote,lambda,set!）是绑定到 `core_forms`，基本过程（如car,cdr,cons等）是绑定到 opcode。

## 语法分析

语法分析主要是由 eval.c 文件中的 `analyze` 函数完成。语法分析接受的输入是scheme的对象，将它们转换成抽象语法树返回。

下面把analyze的伪代码给出来，可以对照着源文件看：

    loop:
    若x是pair
        若不是list  出错
        若car(x)是符号
            在环境中找car(x)的原子cell
            若没找到 当符号闭包处理（宏系统）
            否则 cdr(cell)得到真实操作op
            若op是核心表
                若是define  生成define表的ast
                若是set     生成set表的ast
                若是lambda  生成lambda表的ast
                若是if      生成if表的ast
                ...
            若op是宏    展开宏 跳到loop继续处理
            若op是操作码 处理并生成app的ast
            若是其它 　　生成app的ast
    若x是符号
        生成ref的ast
    若x是符号闭包(宏系统)
        展开宏继续调用analyze
    若x为空  报错
    若是其它  返回对象本身

scheme 的宏系统跟C那种纯粹的代码替换完全不同，是个博大精深的话题，这里不展开。下面借几个抽象语法树(ast)来研究语法分析是如何工作的。

先从最简单的开始，变量引用的ast(在chibi-scheme 中所有的 ast 都被抽象成了 sexp) 在 sexp.h 的 `sexp_struct`的 union 中可以找到其定义：

```C
struct {
  sexp name, cell, source;
} ref;
```

生成变量引用的 ast 的函数是 eval.c 中的 `analyze_var_ref` 函数。假如有 scheme 代码 a ，则对它进行语法分析的过程，就是先找到环境 env，然后在 env 中查找符号 a 绑定的 cell。返回的 ast 就是一个 ref，其 name 就是符号a，cell 就是找到的 cell。有了这个变量引用的 ast，在代码生成的时候就只需要 ast，不用再查环境了。

再看一个复杂一点点的例子：if 表的 ast。if 表生成的 ast是 cnd，在 sexp.h 的 `sexp_struct` 的 union 中可以找到其定义:

```C
struct {
  sexp test, pass, fail, source;
} cnd;
```

source 是这个编译器为支持 debug 弄的，先放一边。其有用的三个字段分别是 test，pass，fail。


if 表的生成函数在 eval.c 文件中的 `analyze_if`，代码如下：

```C
static sexp analyze_if (sexp ctx, sexp x) {  
    ...  
    test = analyze(ctx, sexp_cadr(x));  
    pass = analyze(ctx, sexp_caddr(x));  
    fail_expr = sexp_pairp(sexp_cdddr(x)) ? sexp_cadddr(x) : SEXP_VOID;  
    fail = analyze(ctx, fail_expr);  
    res = (sexp_exceptionp(test) ? test : sexp_exceptionp(pass) ? pass :  
           sexp_exceptionp(fail) ? fail : sexp_make_cnd(ctx, test, pass, fail));  
    ...  
  return res;  
}
```

假设scheme源代码是 `(if (> a 3) b #t)` ，经过语法分析之后，生成的抽象语法树 cnd 的 test 域对应为 `(> a 3)` 的 ast，

pass 和 fail 分别对应到 a 和 3 的 ast。b 是一个符号，所以其 ast 是一个 ref 结构，而 #t 的 ast 是它本身。

## 小结

编译原理的典型步骤，词法分析，语法分析，中间代码生成，代码优化，目标代码生成…这里先写了词法分析。由于lisp的语法简单，词法分析是非常容易的。

词法分析之后插入了一个 scheme/lisp 的重要概念：环境。环境提供了一个符号-值的绑定，在语法分析时，需要通过环境查找符号对应的 scheme 对象。

语法分析接受词法分析的输出作为其输入，生成抽象语法树。语法分析稍微复杂一些，这里给了变量引用和if表的抽象语法树的结构作为例子。作为一篇文章可能有点长，所以我分成二篇来写，后面一篇会继续讲编译原理，给出一个语法分析生成抽象语法树的更复杂的例子。然后进入代码生成的过程。
