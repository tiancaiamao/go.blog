# 作业3

### 内容

1. 背景
2. Scheme子集3
3. 语义
4. 要做的事
	* 4.1. verify-scheme
	* 4.2. finalize-locations
	* 4.3. expose-frame-var
	* 4.4. expose-basic-blocks
	* 4.5. flatten-program
	* 4.6. generate-x86-64	
5. 样板文件和运行时代码
6. 编码提示
7. 测试

## 1. 背景

到目前为止，我们的代码中已经有像rax,r12和fv3这样的变量名。这些一点都不利于记忆。我们需要避免不停地问，或者通过像tests2.ss中的factorial的例子那样动态构造跳转表。这次作业通过提供一个新的locate表部分地解决的是前面那个问题，并通过提供if表达式完全解决后面的问题。

## 2. Scheme子集3

下面是这个星期我们要处理的Scheme子集的语法。

	Program	->	(letrec ([label (lambda () Body)]*) Body)
	Body	->	(locate ([uvar Loc]*) Tail)
	Tail	->	(Triv)
			|	(if Pred Tail Tail)
			|	(begin Effect* Tail)
	Pred	->	(true)
			|	(false)
			|	(relop Triv Triv)
			|	(if Pred Pred Pred)
			|	(begin Effect* Pred)
	Effect	->	(nop)
			|	(set! Var Triv)
			|	(set! Var (binop Triv Triv))
			|	(if Pred Effect Effect)
			|	(begin Effect* Effect)
	Loc		->	reg | fvar
	var		->	uvar | Loc
	Triv	->	Var | int | label

uvar是一个不重名的Scheme变量，类似于标签带一个后缀，不同的是后缀用“.”标记而不是“$.”。也就是，uvar变量是一个符号，命名形式是prefix.suffx，其中suffx是不为空的数字序列并且开头不必没意义的0，比如，f.0，minimal?.3或者destory-instance!.50。在同一个Body内面，每个uvar的后缀必须唯一，但是前缀可以不同，所以f.1和f.2可以在同一个Body内使用，但是f.1和g.1不可以。Body中的每个uvar只能在locate中正好对应到一个绑定。

寄存器(reg)，frame变量(fvar)，标签(label)，整数(int)和二元操作(binop)没变过，跟之前的子集一样。

这个语法依然是受限于x86\_64的目标架构的，在作业2中已经描述过。新的关系操作也类似的受到架构限制。

## 3. 语义

在我们新的子集中的locate表达式看上去很像let表达式，但是不像let那样创建一个新的位置，它只是创建一个从等式左边变量到等式右边位置的别名。换句话说，它说明了每个变量是存储在哪里的。每个变量都必须被赋值到单个位置，因此它只能在locate表中的等式左边出现一次。另一方面，多个变量可以被赋值到相同的位置，也就是，x和y都可以放到寄存器rbx中。当然，程序员需要保证不能多个变量在同一时间使用同一个位置。

我们新的子集中的if表达式很像Scheme中的if表达式，除了谓词表达式是带限制条件的。谓词可以是空元运算(true)，空元运算(false)，二元的关系运算，else部分是谓词的if表达式，或者最后一个子表达式是谓词的begin表达式。

谓词不可以是一个变量引用或者一个procedure调用。原因很简单：我们想让我们的中间语言独立于源语言，这样就独立于特定的true或false的表示了。如果我们想在谓词允许变量引用或者过程调用，我们必须遵守这个表示。

## 4. 要做的事

为了处理新的源语言，我们需要更新验证过程；添加一个新的步骤将每个出现的uvar替换为对应的位置；更新expose-frame-var处理if表达式，谓词表达式，和begin表达式；添加一个新的步骤，expose-basic-blocks，将程序重写成跳转只出现在尾上下文，修改flatten-program和代码生成处理条件跳转。这些新的和修改的步骤如4.1-4.6部分所示。

### 4.1. verify-scheme

这个步骤必须修改以反映新的Scheme子集结构。

### 4.2. finalize-locations

这个步骤将locate表的body部分中出现的每个uvar替换为相应的Loc。它还会去掉locate表。这个步骤的输出的语法如下。

	Program	->	(letrec ([label (lambda () Tail)]*) Tail)
	Tail	->	(Triv)		
			|	(if Pred Tail Tail)
			|	(begin Effect* Tail)
	Pred	->	(true)
			|	(false)
			|	(relop Triv Triv)
			|	(if Pred Pred Pred)
			|	(begin Effect* Pred)
	Effect	->	(nop)		
			|	(set! Loc Triv)
			|	(set! Loc (binop Triv Triv))
			|	(if Pred Effect Effect)
			|	(begin Effect* Effect)
	Loc		->	reg | fvar
	Triv	->	Loc | int | label

例子：源代码

	(letrec ([f$1 (lambda ()
					(locate ([x.1 r8] [y.2 r9])
						(if (if (= x.1 1) (true) (> y.2 1000))
						(begin (set! rax y.2) (r15))
						(begin
							(set! y.2 (* y.2 2))
							(set! rax x.1)
							(set! rax (logand rax 1))
							(if (= rax 0) (set! y.2 (+ y.2 1)) (nop))
							(set! x.1 (sra x.1 1))
							(f$1)))))])
		(locate () (begin (set! r8 3) (set! r9 10) (f$1))))

转化为如下所示。

	(letrec ([f$1 (lambda ()
					(if (if (= r8 1) (true) (> r9 1000))
						(begin (set! rax r9) (r15))
						(begin
							(set! r9 (* r9 2))
							(set! rax r8)
							(set! rax (logand rax 1))
							(if (= rax 0) (set! r9 (+ r9 1)) (nop))
							(set! r8 (sra r8 1))
							(f$1))))])
		(begin (set! r8 3) (set! r9 10) (f$1)))

这个例子精心设计为除了包含一个非空的locate表，还有if表达式中有三种有效的上下文：Tail,Pred和Effect。在新添加的和更新过的步骤中，我们会使用它作为一个例子。

### 4.3. expose-frame-var

你可能会需要修改这个步骤以反映finalize-locations的语法变化。不需要处理locate表，因为在上一个步骤已经去掉了。

如果你之前是使用的一个独立于语法结构的简单的树遍历方式实现，你也可能不需要做任何修改。

### 4.4. expose-basic-blocks

这个步骤的目标是将输入语言精简到跟我们上周flatten-program处理的输入一样的形式，添加了一个限制是if表达式必须是尾上下文中，这样它就等价于两个条件跳转。为了做到这点，必须引入新的标签来处理条件控制流，并将这些标签绑定到letrec顶级作用域中的procedure，这些procedure会表示代码执行时每次跳转的目标。

下面的语法描述了这个步骤将会输出的问题语言。

	Program	->	(letrec ([label (lambda () Tail)]*) Tail)
	Tail	->	(Triv)
			|	(if (relop Triv Triv) (,label) (,label))
			|	(begin Effect* Tail)
	Effect	->	(set! Loc Triv)
			|	(set! Loc (binop Triv Triv))
	Loc		->	reg | disp-opnd
	Triv	->	Loc | int | label

将这个语法和之前的比较，你会发现以下几点。

* if表达式中的then和else部分不再是任意的Tail表达式而是调用(跳转)到标签。
* Pred表达式没有了。唯一剩下的部分是在if表达式中的test部分的relop操作。
* Effect表达式被简化为两种赋值。

我们将这个步骤叫做expose-basic-blocks是因为这个步骤将包含任意嵌套的if和begin表达式输入代码转化为只含有基本块，基本块只有简单的序列，从前面代码进入，从后面代码退出。对于基本块可以做很多有趣的优化，不久之后我们将试着做一两个。

下面示例程序在运行expose-basic-blocks之后的代码：

	(letrec ([f$1 (lambda () (if (= r8 1) (c$8) (a$9)))]
				[c$8 (lambda () (c$6))]
				[a$9 (lambda () (if (> r9 1000) (c$6) (a$7)))]
				[c$6 (lambda () (begin (set! rax r9) (r15)))]
				[a$7 (lambda ()
						(begin
							(set! r9 (* r9 2))
							(set! rax r8)
							(set! rax (logand rax 1))
							(if (= rax 0) (c$3) (a$4))))]
				[c$3 (lambda () (begin (set! r9 (+ r9 1)) (j$5)))]
				[a$4 (lambda () (j$5))]
				[j$5 (lambda () (begin (set! r8 (sra r8 1)) (f$1)))])
		(begin (set! r8 3) (set! r9 10) (f$1)))

仔细地学习这个并确认执行同样的输入程序计算会得到正确的输出语法。

现在我们知道了这个步骤接受的输入和生成的输出是什么样子。最具挑战的总是，更新到新的语法，而又保持输入程序的语义。当你开始时，下面的建议会比较有用。

* 每个处理Tail,Pred和Effect表达式的辅助函数都可能会创建新的标签绑定，所以都需要同时返回一个绑定的链表以及输出表达式。使用values可以返回两个值。使用match的cata关键字或者let-values可以处理多个返回值。使用helper.ss中的unique-label创建标签。
* Pred的辅助函数需要传入"true"和"false"标签。如果Pred辅助函数出现了一个关系操作调用，它需要生成一个到标签的两次跳转。可能有时会简单一点如果出现的是(true)或(false)。
* Effect的辅助函数需要传入一个链表和一个输出表达式，这样，如果是if表达式，它可以将代码和标签打包。

### 4.5. flatten-program

这个步骤必须扩展以处理尾上下文中的条件表达式(if)以及无条件跳转，将它们转化为汇编中等价的单标签比较分支指令。修改还需要考虑到下一个letrec绑定标签，如果有的话，这样就不会产生不必要的跳转。

新的跳转应该是下面两种形式之一。

	(if (relop Triv Triv) (label))
	(if (not (relop Triv Triv)) (label))

flatten-program需要根据跳转目标选择使用哪一种。如果是letrec绑定中接下要处理的标签相同，那么flatten-program应该生成一个条件跳转到另一个，如果必要就在关系操作调用之上加一个not，也就是，当跳到"false"标签的时候。如果跳转目标跟letrec绑定接下要来处理的标签都不同，那么应该生成一个条件跳转到其中一个紧接一个绝对跳转到另一个。例如，(if (< rax 3) (l$1) (l$2))应该生成

	(if (< rax 3) (jump l$1))

如果l$2是letrec绑定的下一个标签，并且

	(if (not (< rax 3)) (jump l$2))

如果l$1是letrec绑定的下一个标签，它应该生成要么

	(if (< rax 3) (jump l$1))
	(jump l$2)

要么

	(if (not (< rax 3) (jump l$1))
	(jump l$2)

如果l$1和l$2都不是letrec绑定的下一个标签。

处理无条件跳转的代码也应该比较跳转目标和下一个要处理的letrec绑定标签。如果是相同的，则应该放弃跳转以避免如下的代码序列。

	(jump 1$7)
	1$7

标签不应该消除，因为它可能是另外一个跳转的目标标签。

有些情况，输出中可能会出现两个连续的标签，也许会像下面这种情况：

	(jump l$7)
	l$6
	l$7

这样也没问题--我们马上会加入一个优化过程阻止这种情况出现。

示例：对于运行例子，flatten-program输出如下所示。

	(code
		(set! r8 3)
		(set! r9 10)
		f$1
		(if (not (= r8 1)) (jump a$9))
		c$8
		(jump c$6)
		a$9
		(if (not (> r9 1000)) (jump a$7))
		c$6
		(set! rax r9)
		(jump r15)
		a$7
		(set! r9 (* r9 2))
		(set! rax r8)
		(set! rax (logand rax 1))
		(if (not (= rax 0)) (jump a$4))
		c$3
		(set! r9 (+ r9 1))
		(jump j$5)
		a$4
		j$5
		(set! r8 (sra r8 1))
		(jump f$1))

### 4.6. generate-x86-64

这个步骤必须修改成可以处理flatten-program生成的条件跳转，使用cmpq和条件跳转指令je,jne,jl,jle,jg和jge。

例子：对于运行例子，generate-x86-64的输出如下，不带样板代码的部分。

		movq $3, %r8
		movq $10, %r9
	L1:
		cmpq $1, %r8
		jne L9
	L8:
		jmp L6
	L9:
		cmpq $1000, %r9
		jle L7
	L6:
		movq %r9, %rax
		jmp *%r15
	L7:
		imulq $2, %r9
		movq %r8, %rax
		addq $1, %rax
		cmpq $0, %rax
		jne L4
	L3:
		addq $1, %r9
		jmp L5
	L4:
	L5:
		sarq $1, %r8
		jmp L1

## 5. 样板和运行时代码

都没变化。

## 6.编码提示

我们再次强烈建议你使用match。看一看作业2的答案学习下如何使用match的cata和扩展的反引号，可以避免直接使用map和append。

## 7. 测试

tests3.ss中有一些有效的和无效的测试用例。你要确保你的编译器能通过至少这些测试。

我们强烈建议你使用前面发的driver来自动化测试过程。