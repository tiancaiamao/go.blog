# 作业7

### 内容
1. 背景
2. 调用约定
3. Scheme子集6
4. 要做的事
	* 4.1. verify-scheme
	* 4.2. remove-complex-opera*
	* 4.3. flatten-set!
	* 4.4. impose-calling-conventions
	* 4.5. uncover-frame-conflict
	* 4.6. pre-assign-frame
	* 4.7. assign-new-frame
	* 4.8. introduce-allocation-forms
	* * .9. select-instructions
	* 4.10. uncover-register-conflict
	* 4.11. finalize-frame-locations
	* 4.12. discard-call-live
	* 4.13. finalize-locations
	* 4.14. expose-frame-var
	* 4.15. expose-basic-blocks
	* 4.16. Iterating
5. 样板运行时代码
6. 测试

## 1. 背景

这次作业我们将对我们的源语言做一项改变：加入非尾调用。尽管只是一个很小的变化，但是它影响了编译器很多地方。

## 2. 调用约定

为了处理非尾调用，我们需要添加一系列额外的调用约定：

* 在非尾调用的时候，调用者负责修改frame指针，以确保被调者的frame在自己的frame上方，并且调用者负责将参数放到被调者的frame中。

* 在返回的时候，被调者负责修改frame指针让它指向原来的frame。

## 3. Scheme子集7

这次我们要处理的Scheme子集的语法在Value和Effect中加入了非尾调用。

	Program	->	(letrec ([label (lambda (uvar*) Body)]*) Body)
	Body	->	(locals (uvar*) Tail)
	Tail	->	Triv
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
			|	(Value Value*)
			|	(if Pred Effect Effect)
			|	(begin Effect* Effect)
	Value	->	Triv
			|	(binop Value Value)
			|	(Value Value*)
			|	(if Pred Value Value)
			|	(begin Effect* Value)
	Triv	->	uvar | int | label

唯一变量(uvar)，标签(label)，整数(int)，二元操作(binop)和关系操作(relop)相比之前的子集没有变化。关于整数的机器限制也依然还在。

## 4. 要做的事

为了处理新的源语言，我们需要对以下步骤做少量的更新：

* verify-scheme
* remove-complex-opera*
* flatten-set!
* select-instructions
* uncover-register-conflict
* assign-registers
* assign-frame
* finalize-frame-locations
* finalize-locations

下面这些需要做较大的更新：

* impose-calling-conventions
* uncover-frame-conflict
* discard-call-live

需要重写：

* expose-frame-var

需要添加：

* pre-assign-frame
* assign-new-frame

丢弃：

* introduce-allocation-forms

并且要对我们的寄存器和frame分配步骤的迭代做一些小的修改。

这看起来好像有很多工作要做，确实也是。幸运的是，总共只有一个步骤assign-new-frame是新的，因为assign-new-frame本质上跟assign-frame是一样的。

新加入的以及更新的步骤在4.1到4.15部分是描述。迭代的变化在4.16部分中描述。

### 4.1. verify-scheme

这步需要加两个简单的match语句来处理Value和Effect中的非尾调用。

### 4.2. remove-complex-opera*

这步需要加两个简单的match语句来处理Value和Effect中的非尾调用。

### 4.3. flatten-set!

这步需要加两个简单的match语句来处理Value和Effect中的非尾调用。

这一步输出的语法如下所示。

	Program	->	(letrec ([label (lambda (uvar*) Body)]*) Body)
	Body	->	(locals (uvar*) Tail)
	Tail	->	Triv
			|	(binop Triv Triv)
			|	(Triv Triv*)
			|	(if Pred Tail Tail)
			|	(begin Effect* Tail)
	Pred	->	(true)
			|	(false)
			|	(relop Triv Triv)
			|	(if Pred Pred Pred)
			|	(begin Effect* Pred)
	Effect	->	(nop)
			|	(set! uvar Triv)
			|	(set! uvar (binop Triv Triv))
			|	(set! uvar (Triv Triv*))
			|	(Triv Triv*)
			|	(if Pred Effect Effect)
			|	(begin Effect* Effect)
	Triv	->	uvar | int | label

### 4.4. impose-calling-conventions

这步也需要添加两个match语句处理非尾调用，而且，它必须记录下它找到的非尾调用的出去的frame变量的链表(如果你这步的版本还没有处理Pred和Effect表达式，这次必须做了)。

对于一个Effect中的非尾，这个步骤执行如下转换：

	(proc e0 ... en-1 en ... en+k-1)
		=>	(return-point rp-label
				(begin
					(set! nfv0 en)
					...
					(set! nfvk-1 en+k-1)
					(set! p0 e0)
					...
					(set! pn-1 en-1)
					(set! ra rp-label)
					(proc fp ra p0 ... pn-1 nfv0 ... fvk-1)))

其中变量nfv0到nfvk-1是新的唯一变量，rp-label是新的标签。

变量列表(nfv0 ... nfvk-1)还必须记录为新的frame之一，放到包在body外面的在new-frames表中(见下面的语法)。最简单的实现方法是把他们加到一个链表中存储，这个链表的变量是一个Body辅助函数可以访问到的变量。所有其它的辅助函数也需要放到Body里面去。

我们使用新的new-frame变量而不是frame变量，因为我们还不知道哪些会是向外流出的变量，即，调用者的frame应该是多大。这会由新的步骤assign-new-frame决定。

new-frame中的变量的分配顺序是无关的，正如寄存器分配顺序那样。然而，所有的new-frame分配都应该在所有寄存器分配之前，这样可以限定参数寄存器的生命期。

对于一个出现在赋值右手边的非尾调用，这步执行的转换本质上跟上面的转换是一样的，除了return-point表后面要跟一个赋值，将左手边的变量赋给返回值寄存器，被调者是将返回值放在这个寄存器中的。

	(set! uvar
		(proc e0 ... en-1 en ... en+k-1))
		=>	(begin
				return-point code, as above
				(set! uvar rv))

做这个的最简单的方式是回调Effect辅助函数来处理，然后用赋值生成begin表达式，使用类似下面的match语句。

	[(set! ,var (,rator ,rand* ...))
		(make-begin
			`(,(Effect `(,rator ,rand* ...))
				(set! ,var ,return-value-register)))]

新的new-frames表将包装在body，为每个body里面的frame列出new-frame变量(有序)。例如：

	(new-frames ((nfv.17 nfv.18 nfv.19) (nfv.20)) tail)

声明tail包含两个return-point表，其中一个有三个传出的new-frame参数(nfv.17, nfv.18和nfv.19)，另一个有一个new-frame参数(nfv.20)。这个列表不需要特定的顺序，但是对列表中每一项，第一个传出参数必须是在第一个位置，第二个在第二个位置，依次类推。

这个步骤还要做的一件事是，将new-frame变量包含到locals表中。如果没有这么做，uncover-frame-conflict将不会在冲突表中为它们创建关联项。

这一步的输出的语法如下所示。

	Program	->	(letrec ([label (lambda () Body)]*) Body)
	Body	->	(locals (uvar*)
					(new-frames (Frame*) Tail))
	Frame	->	(uvar*)
	Tail	->	(Triv Loc*)
			|	(if Pred Pred Pred)
			|	(begin Effect* Tail)
	Pred	->	(true)
			|	(false)
			|	(relop Triv Triv)
			|	(if Pred Pred Pred)
			|	(begin Effect* Pred)
	Effect	->	(nop)
			|	(set! Var Triv)
			|	(set! Var (binop Triv Triv))
			|	(return-point label (Triv Loc*))
			|	(if Pred Effect Effect)
			|	(begin Effect* Effect)
	Loc		->	reg | fvar
	Var		->	uvar | Loc
	Triv	->	Var | int | label

### 4.5. uncover-frame-conflict

这步需要加一个match语句来处理非尾调用，即，return-point表。除此之后，它还要决定call-live变量集合和frame位置。如果一个变量或者frame位置在整个调用过程都是live的，那么它就是call-live的，也就是，在调用返回之后，它的值仍然可能被使用。

call-live的变量和frame位置的集合记录在一个新的call-live表，包装在body外面给assign-new-frame使用。集合将call-live变量(不是frame位置)单独记录在一个spills包装外面，提供给pre-assign-frame使用。

在实际场景中，call-live变量和frame位置是那些在return-point表之后还是live的。将所有的call-live变量集合收集起来最简单的实现方式是用一个Effect辅助函数处理return-point表并且将它们加到一个表示运行集合的变量中存储，这个变量是Body的辅助函数可访问的。所有其它的的辅助函数也需要放在Body中。

// 好长一段不会翻译

在这步的输出中，只有Body的语法有所不同。不同之处在于外面包装添加了spills，call-live和frame-conflict。

	Body	->	(locals (uvar*)
					(new-frames (Frame*)
						(spills (uvar*)
							(frame-conflict conflict-graph
								(call-live (uvar/fvar*) Tail)))))

### 4.6. pre-assign-frame

这一步为所有spills列表中的变量找到对应的frame位置。它跟assign-frame的不同只在于输入和输出表的Body部分。跟assign-frame类似，它消除spills表但是保留其它的表。它还添加了一个locate表，这是在输入中没有的。输出语法的Body如下所示。

	Body	->	(locals (uvar*)
					(new-frames (Frame*)
						(locate ([uvar fvar]*)
							(frame-conflict conflict-graph
								(call-live (uvar/fvar*) Tail)))))

### 4.7. assign-new-frame

对于每个body，这一步决定body的frame的大小，基于变量的位置和call-live列表中的frame变量。它然后为new-frames表中的每个new-frame变量列表选择位置，重写body的代码放到位于body的frame上方的被调者的frame中。

frame的大小很简单，就是call-live变量或者frame变量的frame位置的最大下标加一。frame变量的下标可以直接通过helpers.ss中的frame-var-index取得，而决定变量的下标涉及到先通过locate表找到它被赋值到哪个frame变量。

一旦frame大小n确定之后，每个传出的new-frame变量序列被分配到frame位置的fvn，fvn+1等等。这些被记录到locate表中，跟之前步骤已经做过的分配放到一起。还有，每个return-point表被重写为上推和下压frame指针寄存器，移动nb个字节，nb由procedure的frame元素数目n决定。

	(return-point rp-label tail)	=>
		(begin
			(set! fp (+ fp nb))
			(return-point rp-label tail)
			(set! fp (- fp nb)))

nb可以通过对n左移位得到，左移的量由helpers.ss中的变量align-shift决定。

例如，考虑下面的Body。

	(locals (nfv.17 nfv.18 nfv.19 nfv.20 local ...)
		(new-frames ((nfv.17 nfv.18 nfv.19) (nfv.20))
			(locate ([rp.12 fv0] [x.3 fv2])
				(frame-conflict conflict-table
					(call-live (rp.12 x.3 fv1)
						tail)))))

frame大小是3，因为call-live变量和frame变量的最大的位置下标是2（rp.12下标是0，x.3下标是1，fv1是2。0，2和1中间最大的数是2）。所以每个frame变量的序列从fv3开始分配位置，输出如下面所示。

	(locals (local ...)
		(ulocals ()
			(locate ([rp.12 fv0] [x.3 fv2]
					[nfv.17 fv3] [nfv.18 fv4] [nfv.19 fv5]
					[nfv.20 fv3])
				(frame-conflict conflict-table
					tail))))

不再需要new-frames表和call-live表，所以它们被去掉了。还有，new-frame变量也已经从locals表中去掉了，这个替换过程很简单，只是把new-frame表中出现的变量从locals表中去掉。

还有一个新的表出现：ulocals表是由introduce-allocation-forms插入的，现在我们的编译器只是简单的丢弃它。

在上面例子的tail中，对每个return-point表，frame指针上推和下移的量是3乘以align-shift，或者说是24，如果按helpers.ss中给出的align-shift的标准值计算。

理想情况，我们应该对每个出现非尾调用的地方分别地计算以决定frame大小，而不是考虑“所有的用这个大小就够了”的方式，这会导致一些包含调用的地方frame过大。这需要uncover-frame-conflict提供更多的关于call-live的信息，并且它还会使assign-new-frame复杂得多。所以我们决定不要搞得这么复杂。

### 4.8. introduce-allocation-forms

这个步骤去掉了，即，不再出现在compiler-passes的参数中了。之前由它引入的locate表，现在要由pre-assign-frame引入，并且ulocals表现在是由assign-new-frame引入。

### 4.9. select-instructions

这个步骤需要添加一个简单的match语句来处理Effect中的return-point表。

### 4.10. uncover-register-conflict

这个步骤需要添加一个简单的match语句来处理Effect中的return-point表。

一个非尾调用实际上就是对所有caller-save寄存器的一个的赋值。对于uncover-frame-conflict的情况，我们不需要关心冲突涉及的寄存器。对于uncover-register-conflict，在一个调用之后只有寄存器可能是live的(因为所有的call-live变量都已经被spilled了)，并且我们不关心寄存器对寄存器的冲突。因此，我们不需要担心非尾调用就是赋值这个问题，尽管我们可以检验除了frame指针和返回值寄存器以外，没有其它寄存器在return-point表之后是live的。

### 4.11. finalize-frame-locations

这个步骤需要添加一个简单的match语句来处理Effect中的return-point表。它还需要处理每次调用中的live的位置集合，就像它们是任意的Triv表达式，以方在live位置集合中出现了任何new-frame变量。例如，

	(proc rbp r15 r8 r9 nfv.20)

应该被转换为

	(proc rbp r15 r8 r9 fv3)

如果nfv.20已经被分配到了frame变量fv3。

如果没有完成这个，当nfv.20出现在live set中时，uncover-register-conflict会不知道如何处理。

### 4.12. discard-call-live

这个步骤现在必须处理Effect和Pred表达式来寻找非尾调用，不像之前，所有的调用都是在Tail中的。

### 4.13. finalize-locations

这个步骤需要添加一个简单的match语句来处理Effect中的return-point表。

### 4.14. expose-frame-var

这个步骤现在必须敏感地追踪frame指针寄存器的变化。这个很有必要，因为现在我们会在return-point表之前上推frame指针，然而frame变量可能会出现在return-point表中。出现在代码中的frame变量的下标受到frame指针变化的影响，需要调整相应的量。为了使它更通用，这一步需要能够处理任意位置frame指针的增加和减少，防止优化步骤以某种方式移动或者改变它们，例如，如果两个或者多个非尾调用连续出现时，把frame指针放在一个地方。

需要告知这个步骤使用的每个辅助函数，当前frame指针寄存器偏移是多少。它还需要返回最后的偏移量，做为一个附加的回返值，即，既要返回转换后的表达式，又要返回最后的frame指针偏移。Effect辅助函数需要识别frame指针的增加和减少的量并相应地调整偏移。对于if表达式，then和else部分开始的偏移应该相同，即，test部分之后的偏移量，并且最好检验一下结束时的then和else部分的偏移是相同的。

### 4.15. expose-basic-blocks

这个步骤需要添加一个match语句来处理Effect中的(return-point rp-label tail)表。

它应该将剩下的表达式打包到一个lambda表达式中，并将rp-label绑定到这个lambda表达式，像其它Tail表达式一个处理放到tail内部的表达式，并像其它Effect表达式那样处理"before"表达式（同一个begin表达式中，在return-point表之前的部分，如果有）。

### 4.16. Iterating

由于一些变量(call-live变量)是在迭代开始之前给定的frame位置，我们需要对迭代做一个简单的修改，将finalize-frame-locations移到迭代的最上面，这样每次从最下面过来都会运行它，最下面运行的是assign-frame。在每次迭代之前，以及由pre-assign-frame和assign-new-frame分配最终位置之后，都需要运行finalize-frame-locations。做这个修改并且添加新的步骤之后，compiler-passes参数如下面所示。

	(compiler '(
		verify-scheme
		remove-complax-opera*
		flatten-set!
		impose-calling-conventions
		uncover-frame-conflict
		pre-assign-frame
		assign-new-frame
		(iterate
			finalize-frame-locations
			select-instructions
			uncover-register-conflict
			assign-registers
			(break when everybody-home?)
			assign-frame)
		discard-call-live
		finalize-locations
		expose-frame-var
		expose-basic-blocks
		flatten-program
		generate-x86-64
	))
  
## 5. 样板和运行时代码

都没有变化。

## 6. 测试

在tests6.ss里面有一个小的测试集，包括会通过的和不通过的测试。你需要确保你的编译器步骤至少能够通过这个测试集。

## 7. 编码提示

在tests6.ss里面有一个小的测试集，包括会通过的和不通过的测试。你需要确保你的编译器步骤至少能够通过这个测试集。