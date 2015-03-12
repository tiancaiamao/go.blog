# 作业8

### 内容
1. 背景
2. UIL
3. 语义
4. 要做的事
5. 样板和运行时代码
6. 测试

## 1. 背景

在这次作业中我们将完成我们的"universal intermediate language"(UIL)，这是一个独立于源语言的中间语言，因此可以适用于多种源语言的目标语言。

为了完成UIL，我们需要添加分配存储空间，将数据存储到分配的空间中，和从分配的存储空间中取出数据的primitive。

## 2. UIL

通用中间语言的语法如下所示。相对于我们之前的中间语言，添加了alloc,mref,和mset!的原语。

	Program	->	(letrec ([label (lambda (uvar*) Body)]*) Body)
	Body	->	(locals (uvar*) Tail)
	Tail	->	Triv
			|	(alloc Value)
			|	(mref Value Value)
			|	(binop Value Value)
			|	(Value Value*)
			|	(if Pred Tail Tail)
			|	(begin Effect* Tail)
	Pred	->	(true)
			|	(false)
			|	(relop Value Value)
			|	(if Pred Pred Pred)
			|	(begin Effect* Pred)
	Effect	->	(nop)
			|	(set! uvar Value)
			|	(mset! Value Value Value)
			|	(Value Value*)
			|	(if Pred Effect Effect)
			|	(begin Effect* Effect)
	Value	->	Triv
			|	(alloc Value)
			|	(mref Value Value)
			|	(binop Value Value)
			|	(Value Value*)
			|	(if Pred Value Value)
			|	(begin Effect* Value)
	Triv	->	uvar | int | label

唯一变量(uvar)，标签(label)，整数(int)，二元操作(binop)和关系操作(relop)跟前面作业中描述的中间语言相比没有变化。关于整数的机器限制也跟之前子集一样依然存在。

## 3. 语义

(alloc expr)执行expr生成一个值n，通过将allocation指针寄存器上推保留n字节的存储，并返回存储地基地址。n应该是目标机器字节大小的倍数，allocation指针寄存器由helpers.ss中的变量allocation-pointer-register决定。

(mset! base-expr offset-expr expr)执行base-expr，offset-expr和expr生成变量base，offset和val。它将val存储在base+offset位置。

(mref base-expr offset-expr)执行base-expr和offset-expr生成base和offset。它返回存储的base+offset位置的值。

## 4. 要做的事

需要写一个新的步骤verify-uil，它会永久的成为我们编译器的一个部分，即使在我们创建独立于语言的编译器之后。它的作用是确定语言不独立的部分会生成一个满足规则的UIL代码。这个步骤只需要直接修改上个星期的检验器。

这次作业主要的挑战是，决定为了支持新添加的三个primitive要做哪些修改，除了添加verify-uil以外。讲课的时候我们会讨论所有可能的策略。你也可能会用一些其它你找到的资料，包括来自这门课程现在或者之前的学生的。像往常一样，你必须注明哪些是合作的部分，并且对代码的修改部分必须主要是你自己做的。当你试着执行你的方案的过程中，遇到了问题的时候，指导员会帮助你。但是他们除了课堂上一些适度的讨论外，并不会给你一些策略上的引导。

这个星期的文档非常重要，并且需要很细致地描述你的策略。我们鼓励你尽早做一份草案并且在你实现编译器过程中保持更新。这会帮助你完善思考过程。

## 5. 样板和运行时代码

运行时代码不变，但是样板代码必须将allocation指针寄存器初始化到堆的基地址。在helpers.ss中的emit-program现在生成的样板代码会做这件事。

## 6. 测试

这次作业的一个包含正反用例的小的测试集将在下周发出来，在tests8.ss中，并且可能包含课堂上同学们提交的测试用例。你需要确保你的编译器至少能够通过这系列的测试。