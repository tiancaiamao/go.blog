# 作业5

### 内容
1. 背景
2. Scheme子集5
3. 要做的事
	* 3.1. Iterating
	* 3.2. verify-scheme
	* 3.3. uncover-frame-conflict
	* 3.4. introduce-allocation-forms
	* 3.5. select-instructions
	* 3.6. uncover-register-conflict
	* 3.7. assign-registers
	* 3.8. assign-frame
	* 3.9. finalize-frame-locations
	* 3.10. finalize-locations
4. 样板和运行时代码
5. 测试
6. 编码提示

## 1. 背景

寄存器分配和frame分配的目的是，将变量尽可能地放到寄存器中，如果不能就放到frame中。被放到frame中的变量叫做spilled variables，或者，简单地叫做spills，因为它们不能够放到寄存器集中。Spilled变量不再是寄存器的候选，但是不管什么时候spilled变量放到frame中，都需要添加额外的局部变量作为临时加载和存储spilled变量。例如，在x86\_64中，下面例子中如果x和y都是spilled的

	（set! x (+ x y))

必须为x或者y引入一个临时变量，即

	(set! u y)
	(set! x (+ x u))

这种临时变量叫做unspillable变量，因为它们必须被放到寄存器里。在任何时间，活着的unspillable不能够超过可用的寄存器数量。寄存器分配结构时，一些原来的局部变量以及所有的unspillable变量都是分配到寄存器，而剩下的一些局部变量则是放在frame中。

寄存器和frame分配器的主要部分是一个迭代过程，重复地尝试将尽量多的变量分配到寄存器中，并将不能够放入的进行拆分。

在迭代过程之前还要做一准备工作。

1. 程序被重写为反映目标架构的调用协议和运行时系统，即，寄存器和变量参数是显示指定的。在这个过程中，外出的非尾递归frame参数将是未指定的，因为非尾递归调用的frame大小还不能确定。

2. 构造一个frame的冲突图，将每个局部变量关联到一组跟它冲突的其它局部变量和frame位置。在这个过程中，悲观地假设所有的变量都是会放到frame中，所以这些集合将局部变量和每个其它固定frame变量关联起来。构造这个冲突集合要涉及到live analysis。

3. 变量生命期超过非尾调用的变量(也称作"call-live"变量)被当作是spills，并且每个都会分配到一个frame位置。

4. 基于call-live变量的位置，确定每个非尾递归调用的frame参数的位置。每个块都是放到这种生命期会跨越相应调用的变量的上方。frame指针需要上移和下移的量也是在这个时刻确定的。

迭代步骤自身又由下面四个子任务组成。

5. 识别每个操作使用的指令，必要的时候引入unspillable变量，即，在需要寄存器的时候将内存操作替换为寄存器操作。将Spillable和unspillable都乐观地假设成寄存器。

6. 构造一个寄存器冲突图，将每个变量关联到一个链表，链表中是跟它冲突的变量和寄存器。

7. 尝试将每个unspillable和spillable分配到寄存器。如果成功，迭代过程结构，编译器继续寄存器/frame分配之后的过程。否则，将一个spillable变量拆分，继续进行frame分配的迭代。

8. 每个spill被分配到一个frame位置，使用生成的frame冲突和移动集开始新一轮的迭代。处理过程回到第5步。

我们新的步骤处理任务2，5和8。6和7是在上次作业中处理。我们写测试用例实际是是用手动执行1，3和4，但是我们将会在接下来的作业中修改编译器来处理这些任务。

## 2. Scheme子集5

这个星期我们将要处理的Scheme子集的语法跟上个星期是相同：

	Program	->	(letrec ([label (lambda () Body)]*) Body)
	Body	->	(locals (uvar*) Tail)
	Tail	->	(Triv Loc*)
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
	Loc		->	ret | fvar
	Var		->	uvar | Loc
	Triv	->	Var | int | label

唯一变量(uvar)，寄存器(reg)，frame变量(fvar)，标签(label)，整数(int)，二元操作(binop)，和关系操作(relop)都跟上次的子集一样没有变化。

唯一的不同是，在这个子集中，语法表达式不再受限于所有的x86\_64目标架构，因为我们新的指令集步骤过程会将代码重写为符合限制。

然后，还是有两个限制：sra的第二个操作数必须是精确的常数k，0<=k<=63，所有其它的整数常量都必须是精确整数，-2^63<=n<=2^63-1。

## 3. 要做的事

为了处理新的源语言，我们需要更新检验器(去掉大多的目标机器限制的检测)，对于

	uncover-register-conflict

做一些微不足道的修改。对于

	assign-registers
	finalize-locations

做少量的修改。添加几个新的步骤：

	uncover-frame-conflict
	introduce-allocation-forms
	select-instructions
	assign-frame
	finalize-frame-locations

看起来要做好多东西，不过幸好除了两个步骤以外，其它的都是修改旧的步骤。两个新的步骤是introduce-allocation-forms和select-instructions。前面一个很简单，不过后面那个有点挑战。修改已有的步骤都很直观。步骤3.1操作这些步骤是如何一起工作来执行迭代的寄存器和frame分配过程的，新的和修改的步骤在3.2到3.10中描述。

### 3.1. 迭代

如果寄存器分配没能将所有变量分配到寄存器，那么会拆分一些放到frame中。如果需要加载和存储spills，会引入unspillable变量，会有一些frame位置，然后分配器退出。这个过程会重复直到寄存器分配成功。这通常需要一到三次迭代，不过实际的迭代次数是不定的。

到刚才为止，我们可以使用一个扁平的步骤的链表来描述编译器器的驱动的结构。但是现在，我们必须使用迭代来表示寄存器分配和frame分配过程。为了这么做，我们使用更通用的形式作为compiler-passes的参数，按下面形式调用。

	(compiler-passes '(spec ...))

每个spec必须是下列表示之一。

* pass-name：这种表示告诉驱动，依次地运行spec...，使用前面一个的输出作为后面一个的输入。

* (iterate spec ...)：这种表示告诉驱动重复地运行spec ...，，每次使用前面一个的输出作为后面一个的输入。

* (break when predicate)：这个告诉驱动如果将predicate用在当前中间代码时返回true，则退出当前的迭代(如果不在iterator表中则是退出编译器)。

例如，

	(compiler-passes '(a (iterate b (break when c?) d ) e))

会让驱动运行a，并将输出给到b。如果用b的输出调用c?返回真，那么驱动将b的输出传给e；否则，将b的输出传给d，接着将d的输出b，然后测试c?，如果往复。

在我们的编译器中，我们需要迭代直到每个lambda表达式都是完全的，即，所有的变量都分配到了寄存器或者frame位置。可能lambda表达式中有些变量运行一二遍之后就分配好了，而其它的可能需要运行两遍以上。为了确认所有的lambda表达式都是完全的，我们使用下面的预测过程everybody-home?。

	(define-who everybody-home?
		(define all-home?
			(lambda (body)
				(match body
					[(locals (,local* ...)
						(ulocals (,ulocal* ...)
							(spills (,spill* ...)
								(locate (,home* ...)
									(frame-conflict ,ct ,tail))))) #f]
					[(locate (,home* ...) ,tail) #t]
					[,x (error who "invalid Body ~s" x)])))
			(lambda (x)
				(match x
					[(letrec ([,label* (lambda () ,body*)] ...) ,body)
					 (andmap all-home? '(,body ,body* ...))]
					[,x (error who "invalid Program ~s" x)])))

当assign-registers成功地将寄存器分配到所有变量，它会在locate表中记录寄存器位置，以及已经记录在这里的frame位置。它还会丢弃locals，ulocals，frame-conflict和register-conflict表，因为用不着了，只会剩下locate表。

这样，everybody-home?简单地查看lambda的body或者letrec的body中是否有其它的表，如果没有了，说明每个都分配好位置了。

一旦我们有了这个predicate之后，我们可以为驱动定义我们的编译过程如下。

	(compiler-passes '(
		verify-scheme
		uncover-frame-conflict
		introduce-allocation-forms
		(iterate
			select-instructions
			uncover-register-conflict
			assign-registers
			(break when everybody-home?)
			assign-frame
			finalize-frame-locations)
		discard-call-live
		finalize-locations
		expose-frame-var
		expose-basic-blocks
		flatten-program
		generate-x86-64
	))

### 3.2. verify-scheme

这个步骤必须修改，去掉或调整一些对于满足机器限制的检验。

### 3.3. uncover-frame-conflict

这个步骤为每个变量记录一个集合，集合中是跟它冲突的其它变量或frame变量。这个步骤跟uncover-register-conflict是类似的。唯一的不同是uncover-register-conflict中检查是否是寄存器使用的是register?，而uncover-frame-conflict查找frame变量是使用frame-var?检查的。你可能会想要抽象一下这部分的代码，使用不同的判断函数就可以做成一个通用的了。

frame冲突是记录在一个frame-conflict表中，包围body部分。这样，语法变化只有Body部分有一点点，看起来是这样子的。

	Body	->	(locals (uvar*)
					(frame-conflict conflict-graph Tail))

### 3.4. introduce-allocation-forms

在每步寄存器分配和frame分配的迭代中，指令选择过程会将需要分配位置的unspillable变量添加到变量集中，而frame为spilled变量分配位置。前者记录在ulocal表中，而后者记录在locate表中。

由于这些表是在第二轮迭代以后出现的，它们可以很方便地放在第一轮迭代。所以introduce-allocation-forms简单地为body部分加上一个空的ulocal表以及一个空的locate表。这步影响到的语法如下。

	Body	->	(locals (uvar*)
							(ulocals ()
								(locate ()
									(frame-conflict conflict-graph Tail))))

这是一个简单的步骤，只需要对Body进行操作而不需要处理Body内部的Tail表达式。

### 3.5. select-instructions

除了特殊限制哪些寄存器在哪些场景下可以使用，机器架构还限制了常量操作的操作数的大小，以及内存操作里能出现的常量的大小。这些限制不应该影响到我们高层源语言的表达能力，因此，编译器会在某种程度上重写一些输入代码让它服从目标架构。有些时候，这个重写过程需要编译器引入一些必须分配到寄存器的临时变量，因此这一步处理的逻辑应该是出现在寄存器分配步骤之前。

尽管在代码生成过程运行之前没有会产生实际的指令，重写代码使它服从目标架构的限制，这个处理一般叫做指令选择。

这步过程会假设每个uvar变量都会分配一个寄存器。如果不是这样，这一步会在变量被寄存器分配过程拆分并替换成frame变量之后，再次运行。

这一步输出语言的语法跟输入语言的语法是相同的，但是在这一步中我们丢弃了源语言中架构的限制，之后就跟上次的作业着不多了。

这一步的代码没办法写得很优雅，因为它必须符合怪异的x86\_64架构。我们建议你让每个辅助函数返回两个值，重写后的表达式和插入到重写后代码中的unspillable集合。使用helpers.ss中的unique-name创建新的unspillable变量。在我们的实现中，我们使用(unique-name 't)，这样每个unspillable看起来都是t.n的形式，不过只要它们是uvar，实际的名字都不重要。还有，当你需要创建begin表达式的时候，可以使用helpers.ss中的make-begin，这个就不会弄成嵌套一些不必要的begin表达式。

考到+,*,logand和logor的交换性，你大概想尽量地避免生成没必要的upspillable。例如，如果出现(set! x.5 (- 3 x.5))你必须将它转换成像下面这样：

	(begin
		(set! t.6 3)
		(set! t.6 (- t.6 x.5)
		(set! x.5 t.6)))

其中t.6是新的unspillable。但是对于(set! x.5 (+ 3 x.5))，其实你可以简单的交换一下操作：

	(set! x (+ x 3))

类似地，对于一些关系运算，通过交换比较操作也可以避免一些不必要的unspillable，比如把(< 3 x.5)变为(> x.5 3)。

上个星期写的verify-scheme大概可以作为select-instructions要做哪些测试的一个参考。

对于introduce-allocation-forms这一步的输入语言和输出语言的语法有少量的差别，因为这一步的输入也可能是来自于之前的迭代过程中的寄存器分配步骤。如果发生这种情况时：可能会有一个或者多个body部分，寄存器或者frame分配不是完成状态，但是其它部分是完成的。因为任何两个不同的body部分需要的迭代次数可能是不同的。而且，ulocals表中的unspillable和local表中的绑定链表可能是非空的。在Body关键字可以反映出来。

	Body	->	(locals (uvar*)
					(ulocals (uvar*)
						(locate ([uvar fvar]*)
							(frame-conflict conflict-graph Tail))))
			|	(locate ([uvar Loc]*) Tail)

首先，没完成是指，Body表还需要寄存器和frame分配器做更多的工作；其次，完成是指，对于已经分配了位置的变量是完成了的。对于后一种情况，这个步骤需要简单地在输出中重新生成body表。

这一步的输出语言语法跟输入语言语法是一样的。但是如上所述，这个步骤的输出程序会符合x86\_64的架构限制。

### 3.6. uncover-register-conflict

跟之前作业的处理一样，这个步骤的目的是为每个变量记录下跟它冲突的变量和寄存器。这个版本表面上有一些差异是，现在的Body表达式中有更多的包装层，像上面给出的Body语法那样。

对于一个完成的Body，即，只有locate包装，这个步骤只需要将body表重新输出。对于一个未完成的body，需要处理Tail表达式并像之前版本的处理那样插入register-conflict表。输出语言的语法跟输入语言的语法不同之处只有在未完成的Body中出现register-conflict表。

	Body	->	(locals (uvar*)
					(ulocals (uvar*)
						(locate ([uvar fvar]*)
							(frame-conflict conflict-graph
								(register-conflict conflict-graph Tail)))))
			|	(locate ([uvar Loc]*) Tail)

### 3.7. 寄存器分配

这一步跟上次作业相比，有三处修改。

* Body的语法变成了uncover-register-conflict中的那种形式

* 寄存器分配算法必须要考虑到ulocals表中的unspillable变量

* 当变量拆分后，输出需要产生一个spill的链表，使用一个新的spill包装而不是抛出错误

输入语法跟uncover-register-conflict的输出语法相同。再次，这一步需要为完成的body重新生成输出。

对于未完成的body，产生的输出取决于是否能够为所有locals和ulocals表中列出的spillable和unspillable找到一个寄存器。如果是，它应该产生一个完成的body，只有locate表列出原始的frame位置和新的寄存器位置。否则，它应该生成一个未完成的body，在body的spills表中包含spilled变量的链表。它不允许记录能够使用的分配，因为下一轮迭代中分配可能发生变化。然而，它必须从locals表中去掉spilled的变量，因为它们不再是寄存器分配的候选目标。spills集合和unspillables应该是正交的，并且它们的并集应该是跟原始的spillables集合一样的。unspillables不需要改变。这个步骤还必须去掉register-conflict表，因为在下一轮迭代中肯定会变化的。输出语言的语法如果。

	Body	->	(locals (uvar*)
					(ulocals (uvar*)
						(spills (uvar*)
							(locate ([uvar fvar]*)
								(frame-conflict conflict-graph Tail)))))
			|	(locate ([uvar Loc]*) Tail)

这个步骤必须将ulocals表中的unspillable变量放到它要分配寄存器的变量集合中。当被迫从图中删除掉度较高的结点时，它还必须避免选择unspillable变量，因为unspillable变量必须获取寄存器。这是可以保证的，只要提供了少量的寄存器，因为unspillable变量的生命期很短，只会和其它很少的unspillable变量冲突。

### 3.8. assign-frame

如果assign-registers没有成功为所有的局部变量分配寄存器，这个步骤将会被调用。它处理的是由assign-registers生成的spills包装中的集合，为它们分配frame位置。

frame分配跟寄存器分配是类似的，但是要简单一点，因为所有的结点度都很低，因为frame空间的大小可以看作是无限的。这意味着不会出现像拆分那样的情况。它还意味着我们可以立即简化图中的结点，即，我们可以跳过简化过程并直接进行选择。

另一方面，寄存器分配中的简化过程可以看作是一种为了尽量分配到寄存器而将变量进行排序的方式，即，减少spills数量，而不是随便顺序。将frame分配器中的输入变量进行排序可能也有意义，你可能想实验一个不同的策略看看哪一种最好。例如，如果我们记录变量间的移动，那么我们希望为spills首先选择frame中直接或间接跟移动相关的位置。这样可以减少变量冲突的可能性。

选择步骤将spill的链表中的第一个变量分配到一个相容的frame位置(frame变量)。对一个变量x，如果x不直接和frame变量fv冲突(fv不在冲突集中)，并且不是x和变量y冲突，y之前分配到了fv，则x和fv相容的。要判断后面一种情况，必须要将上一轮分配的frame变量分配情况予以考虑，上一轮分配结果是记录在locate表中。

要找到相容的位置的最简单的方式是，检测x不能分配到哪些frame变量，得到一个集合，然后从fv0开始往后面循环，找一个不在集合中的。在helpers.ss中的index->frame-var可以简化这项工作。

这个步骤要将分配结果加入到已有的locate表中。输出的语法跟assign-registers的语法是一样的，除了spill表被丢弃了。

	Body	->	(locals (uvar*)
					(ulocals (uvar*)
						(locate ([uvar fvar]*)
							(frame-conflict conflict-graph Tail))))
			|	(locate ([uvar Loc]*) Tail)

### 3.9. finalize-frame-locations

这个步骤是在assign-frame之后被调用的，它将基于locate表，把每处frame中分配的变量替换为相应的frame变量。本质上是跟finalize-locations一样的，除了Body依旧是有完成的和未完成的形式，原来的Body表都是完成形式。还有，在这一步并不丢弃locate表，因为在下一轮处理中需要考虑这一轮迭代中frame变量分配情况，如果有变量拆分。如此一来，这个步骤的输出语言的语法是跟之前一步的输出语言语法一致的。

这步需要改的一点点地方是，它必须要识别会产生这种赋值情况：

	(set! fvi fvi)

如果会产生这种赋值，则应该替换为(nop)，因为这个赋值毫无意义。而nops在后面的expose-basic-blocks中会被删除。

### 3.10. finalize-locations

这个步骤唯一的改变是避免将一个位置移给它自己，即，下面形式的赋值：

	(set! loc1 loc2)

其中loc1和loc2在替换之后是同样位置。例如，

	(begin
		(set! x 15)
		(set! x (+ x 5))
		(set! x rax)
		(r15))

变为	

	（begin
		(set! rax 15)
		(set! rax (+ rax 15))
		(nop)
		(r15))

如果x是分配到rax的。

即使locate表可能为一些变量列出了frame位置，所有的frame替换都应该在寄存器和frame分配过程中被finalize-frame-locations处理过了，因此这一步中进行的替换应该只有寄存器替换。这个不影响之前的代码，尽管你可以利用这项条件创建一个只包含寄存器赋值的环境。

## 4. 样板和运行时代码

都没有变化。

## 5. 测试

在test5.ss中有一个这次作业的小的测试集，包括有效的和无效的。你需要确认你的编译步骤至少能通过这个测试集。

## 6. 编码提示

在开始之前，先研究一下在线编译器对几个例子的输出，包括那些包含多个body的，其中一些需要超过一次的寄存器或frame分配的迭代。下面是这样一个例子：

	(letrec ([f$1 (lambda ()
					(locals (x.1 y.2 z.3)
						(begin
							(set! x.1 1)
							(set! y.2 2)
							(set! rax (+ x.1 y.2))
							(r15 rax rcx rdx rbx rbp rsi r8 r9 r10
								r11 r12 r13 r14))))])
		(locals () (f$1 rbp r15)))

不要一次处理所有的修改，我们建议你修改现有的步骤；添加introduce-allocation-forms表；在原来基础上修改uncover-frame-conflict，select-instructions，assign-frame，和finalize-frame-locations表。然后用没使用到新步骤的程序测试，确认程序没有违反架构限制并且没有spill任何变量。上次作业中任何有效的测试应该都可以。

一旦你确认这里能工作了，你可以一次一个地将你的版本替换成新的。你大概希望将select-instructions留到最后，因为其它的都相对直观，但是要明白的是，原来的程序不违反限制，并不代表在变量被拆分后也不违反。



