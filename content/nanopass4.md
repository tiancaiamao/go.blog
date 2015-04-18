# 作业4

### 内容

1. 背景
	* 1.1. Live Analysis
	* 1.2. 寄存器分配
	* 1.3. Live Set表示
	* 1.4. 冲突图表示
2. Scheme子集4
3. 语义
4. 要做的事
	* 4.1. verify-scheme
	* 4.2. uncover-register-conflict
	* 4.3. assign-registers
	* 4.4. discard-call-live
5. 样板和运行时代码
6. 测试
7. 编码提示

## 1. 背景

这次作业，我们把局部变量的寄存器分配加入到编译器中，同时也允许手动地分配和引用寄存器和frame变量。编译器将使用图着色的寄存器分配算法。

图着色寄存器分配算法是基于图着色问题，为图(graph)中直接相连的结点加上不同的颜色，就像我们为一张地图中，位置相邻的区域染上不同的颜色。在基于图着色的寄存器分配算法中，图中的结点是变量和寄存器，结点之间的边代表变量和寄存器间的冲突，颜色是特定的寄存器，比如，rax或r12。这个想法是我们要将不同的寄存器分配到冲突的变量，但是要尽量将不冲突的打包到一起，这样我们可以把尽量多的变量存放在寄存器中。

两个变量冲突，意味着我们不能为它们分配同一个寄存器，一个变量和一个寄存器冲突意味着我们不能将这个变量分配到这个寄存器。算法的第一步是检测哪些地方存在冲突。这个过程通过live analysis实现。

### 1.1. Live Analysis

两个变量冲突，或者一个变量和一个寄存器冲突，如果：

a. 在其中分配的时候，另一个正在使用中(live)

b. 不是简单的从一个到另一个赋值

为什么是这样子呢？假设我们分配两个变量x和y到同一个寄存器。如果在整个使用到x的指令的生命期间，没有分配过y，那么我们知道把y分配到同样寄存器并不会消除掉x。然而，如果在使用到x的指令的生命期间，y被分配，则会消除掉x的值。

在任意时刻，一个变量或者寄存器，如何程序可能还会需要使用它的值，则称为使用中，或者叫做live。一般来说，这是一个不可预测的属性，所以我们保守地假设，如果我们不能证明它是not live的，那它就是live的。这里我们使用的传统的保守的方法是，一个变量，从首次出现引用它的地方，直到被其它变量赋值(覆盖)掉，这期间任何时刻该变量都是live的。

考虑下面这个例子。

	(begin
		(set! a r8)
		(set! b fv0)
		(set! c (+ a 2))
		(if (< c 0) (nop) (set! c (+ c b)))
		(set! rax (+ c 1))
		(r15 rax rbp))

在变量a分配的时间点，变量b和c都不是live的。现在b和c这些变量中如果有值的话，都不能被使用，因为它们会被分配覆盖掉。

在变量b分配的时候，a是live的，因为前面时间点有引用它，但是c不是live的。

在变量c分配的时候，b是live的，但是a不是。

由于在b分配的时候a是live的，并且不是直接将b赋值给a，a和b冲突。类似地，b和c冲突，因为分配c时b是live的。但是a和c之前不冲突，因为一方在赋值的时候，另一方都不是live的。这样，可以把a和c放在同一个寄存器里面，但是a和b，或者b和c不行。

由于变量是否live由它是否还会被使用决定，live analysis需要从计算的控制流的叶子从后往前进行的。我们会一次一个procedure地执行live analysis，在我们这边叶子都是尾调用，locations中的list在这时刻都被当作是live的。随着analysis从尾调用往前进行，当遇到变量和寄存器被引用的时候会将它们添加到列表中，当遇到变量和寄存器被赋值的时候，会将它们从列表删除。在处理if表达式的时候，它会分别地计算then和else部分。因为我们不确定会执行哪一边，只要有一边是live的就认为是live的。所以很自然地把这两者放到一起在test部分后面处理(可以做得更加智能一点的，后面再讨论)。

假设上面代码是整个procedure的body部分，live analysis自底向上处理过程如下。

1. (r15 rax rbp)live set初始化为{rax rbp}，因为它们在尾调用的地方是live的。
2. (r15 rax rbp)加入r15，因为尾调用引用了它。
3. (set! rax (+ c 1))对rax的赋值操作干掉了rax，将它移出live set。这一步没有添加冲突集(conflicts)，因为此时运算左边是一个寄存器，而现在冲突集中没有变量。
4. (+ c 1)由于赋值右边引用了c，我们将c加入到live set。
5. (set! c (+ c b))if表达式的else部分中，对c有赋值。在c赋值时，r15和rbp是live的，所以我们添加一个冲突记录c和{r15,rbp}冲突。
6. (+ c b)因为(+ c b)中有引用到c和b，所以c又加回到live set，还有b也加入。
7. (nop)不添也不删除任何东西，所以set都没变化。
8. (if (< c 0) (nop) (set! c (+ c b)))在做小于操作的时候，要把then和else两种情况合到一起使用。
9. (< c 0)小于操作引用c，要将c加入live set，不过c已经在里面了，不用改变。
10. (set! c (+ a 2))c的赋值操作，把c移出冲突集。c赋值时r15,rbp,b都是live的。之前已经知道r15和rbp都和c冲突，现在我们知道b也和c冲突。
11. (+ a 2) a加入live set
12. (set! b fv0) 赋值操作将b移出live set。b赋值时，r15,rbp和a都是live的，所以添加冲突记录b和{r15,rbp,a,c}，a和{b}
13. (set! b fv0) 操作右边是frame变量，忽略不管
14. (set! a r8) 对a赋值操作将a移出live set。此时r15,rbp是live的，所以添加冲突记录a和{r15,rbp}
15. (set! a r8) 右边引用了r8，所以现在live set是rbp,r15,r8。如果这里面出现了非寄存器的情况，那就表明代码里有bug。

现在live analysis完成，得到最终的冲突集：

	a: {r15,b}
	b: {c,r15,a}
	c: {r15,b}

有些需要注意的重要事项：

* 变量绝不会出现在它自身的冲突集中。因为要出现这种情况，它必须在赋值的时候是live的，然而一旦赋值，它又不是live了。通过使用使用live集合在计算赋值右操作和赋值操作本身，可以避免重复地将自身从冲突集中删除。例如，处理(set! c (+ c b))，首先我们将c从live set中移除，然后记录下c和任何live set中寄存器或变量的冲突，然后将c和b加回去。

* 我们没有为寄存器维护冲突集。寄存器是固定的；例如，我们不能决定将r8放到rax。所以寄存器分配决不会询问其它的寄存器或者变量是否和它冲突。

* 在上面的例子中，可能会出现a分配到r8的时候，r8不是live的。即使r8是live的，我们也不会将r8加到a的冲突集，因为这是直接从一个到另一个的赋值。

* 在分配时我们记录的冲突集只依赖于左边变量和正在赋值发生之前的live set中的变量。在检测冲突的时候右边引用到的变量集合是不相关的。因此当我们看到(set! c (+ c b))我们并不会认为a跟b和c冲突。

* 变量可能会在某个不是live的时候被赋值。如果出现这种情况，我们应该丢弃这个赋值，并且忽略掉赋值操作右边的任意变量和寄存器引用。如果我们这样做，通常可以得到最少的冲突。这个没问题，尽量这跟之前说的不一样，但其实这是一个优化过程并丢弃了没用到的赋值。一般来说，我们并不想因为优化而使我们的步骤变复杂，并且我们想给用户选项决定是否允许一些优化。

简单的将if表达式的then和else结合到一起处理，尽管这么做没什么问题，如果if的测试部分是(true)或(false)常量，我们分开来处理可以得到更精确的live信息。对于所有的if表达式，我们为predicate处理传递两个live set，一个true live set(来自then部分的处理)和一个false live set(来自else部分的处理)。处理过程中如果有嵌套的if或者begin中的最后一个表达式，将传递这两部分。如果发现了(true)或(false)，会分别设置成true live set或者false live set。其它情况下，必须要在处理调用之前将两者合并，因为原语调用的布尔结果是不可知的。

当变量引用出现在永远不会执行的代码中，做这个工作是值得的，因为整个测试表达式是一个常量，即，下面的情况。

	(if (true) e1 e2)
	(if (false) e1 e2)

第一种情况，整个if表达式中，只有出现在e1中的变量才有可能是live的，而第二种情况中，只有e2中的变量才有可能是live的。如果这是仅有的一项好处，我们也不担心，因为我们假设这样的表达式已经会前面的步骤优化掉了。事实上它还会在像下面这种情况也很值得。

	(if (if e1
			(begin e2 (false))
			e3)
		e4
		e5)

因为如果代码走的是e2分支，那么e4就绝对不会执行，在e2中的live的变量只可能是出现在e5中。在e2之后出现了(false)，通过只返回false live set，我们可以确保只有这些才是它需要处理的变量。

### 1.2. 寄存器分配

寄存器分配算法接受两个输入：一个变量链表和一个冲突表。它返回一个所有或者部分变量的寄存器分配的链表。最递归可以最简单的描述如下。

* 如何变量链表为空，返回一个空的寄存器分配链表。
* 从变量链表中选择一个low-degree的变量(可拆分或不可拆分)，如果存在。否则任意选择一个可拆分的变量。low-degree变量是当前冲突表中，冲突的变量或寄存器数量小于k个的变量。

* 重复变量选择过程，将变量从链表中删除，并将选出的变量以及和它的冲突链表从冲突表中移除。(这样，递归调用会将寄存器分配到更短的链表，对应的冲突图也会有更少的冲突。)这个递归调用应该返回一个为(至少某些)剩下的变量分配的寄存器分配链表。

* 尝试为挑出来的变量选择一个寄存器，不要选择和这个变量冲突的寄存器，也不要选择分配给了在递归调用返回的寄存器分配链表中的任何寄存器。如果有一个low-degree变量可挑选，则这一步成功，否则不成功。如果成功，将这次分配加到寄存器分配链表中然后返回新的链表。否则返回原来的链表。

这个算法是改编自“Improvements to graph coloring register allocation”中描述的一个乐观寄存器分配算法。

如果寄存器分配器返回的寄存器分配链中包含了每个原始变量集中的变量，那么寄存器分配器就成功了。否则，寄存器分配失败，在原始变量集中但是不在返回链表中的每个变量，必须拆分，即，分配到frame中。现在我们还暂时不处理frame中的分配，因此这种情况寄存器分配器只要简单的报错就行了。

### 1.3. Live Set表示

一个live set在Scheme中最容易表示为一个没有重复元素的变量和寄存器的链表。对于集合有更高效的表示，如果是产品级的编译器，我们需要使用一个，不过这个不是我们的目的。最新的helper.ss中包含了处理集合的procedure：set-cons，添加一个元素到集合，union，将两个或多个集合合并，intersect，求两个或多个集合的交集，difference，返回两个集合不同的部分。

### 1.4. 冲突图表示

在Scheme中冲突图最容易表示为一个关联链表将每个变量映射到一个跟它冲突的变量和寄存器链表。例如，

	((a r15 rbp b)
	 (b r15 rbp a c)
	 (c b r15 rbp))

这个例子中冲突集的表示跟前面一节相同。这个表示有点冗余，因为变量的冲突出现在list中两次，每个变量都有一次，不过维护这个冗余信息可以让处理冲突变量高效一些。

这种形式表示的集合的操作可以使用assq和set-cdr!。

再次，我们大概也可以在产品级的编译器中选择一个更高效的表示，不过这不是我们的目的。

## 2. Scheme子集4

这里是我们这周处理的Scheme子集的语法法。

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
	Loc		->	reg | fvar
	Var		->	uvar | Loc
	Triv	->	Var | int | label

改变的地方是，locate表被替换成了locals表，这个表列举变量，但是没有位置；现在每个尾调用的地方都包含一个位置链表Loc*。

唯一变量(uvar)，寄存器(reg)，frame变量(fvar)，标签(label)，整数(int)和二元操作(binop)跟之前相比没有变化。

Body中的每个uvar在locals表中应该只出现一次。

语法法表达式依然受到x86\_64目标架构的限制，见前面作业中的描述。

## 3. 语义

locals表类似locate表那样声明了唯一变量。然而，它不指定一系列的位置，因为编译器要负责为locals表中的变量分配位置。

假定每个调用中位置链表Loc*指定了跳转目标的位置，对于表示返回的调用，它通常会包含返回值寄存器，rax。对于表示尾调用的调用，它包含了参数的位置。两种情况，rbp都应该在列表中，除非整个程序都不使用frame变量。链表中位置的顺序并不重要。

## 4. 要做的事

为了处理新的源语言，我们需要更新验证步骤，并添加三个新的步骤：一个执行live analysis并构造一个冲突图，一个分配寄存器，还有一个去除每个调用中的Loc*链表。新加入的三个过程在verify-scheme后面，在finalize-locations前面。新的和修改过的步骤在4.1-4.4部分中描述。

### 4.1. verify-scheme

这个步骤要修改反映新的Scheme子集结构的变化。

### 4.2. uncover-register-conflict

这个步骤为位置链表中的每个uvar插入一个冲突图链表，链表中是跟它冲突的uvar和寄存器，即，不能跟它共享的寄存器。程序的其它地方不需要改变。

这个步骤的输出的语法如下所示。

	Program	->	(letrec ([label (lambda () Body)]*) Body)
	Body	->	(locals (uvar*)
					(register-rconflict conflict-graph Tail))
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
	Loc		->	reg | fvar
	Var		->	uvar | Loc
	Triv	-> Var | int | label

冲突图应该是一个关联链，将locals中每个变量映射到一个变量和寄存器的链表，见1.4部分里的描述。

### 4.3. 分配寄存器

这一步尝试为locals链表中的每个uvar分配寄存器，使用1.2部分中描述的寄存器分配算法。可用的寄存器在helpers.ss中给出。

如果成功了，它会将记录寄存器分配的结果放到locate表中，并删除locals和register-conflict表。这一步不改变程序其它地方。

这个步骤的输出的语法跟作业3中的源语言语法几乎是一模一样的。唯一的不同是Loc*链表出现在每个调用中，接下来的步骤会将去除这个链表，并用它替换locate绑定中的寄存器。

	Program	->	(letrec ([label (lambda () Body)]*) Body)
	Body	->	(locate ([uvar reg]*) Tail)
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
	Loc		->	reg | fvar
	Var		->	uvar | Loc
	Triv	-> Var | int | label

### 4.4. discard-call-live

这个步骤将去掉每个调用中的Loc*链表。这个步骤的输出是跟作业3中的源语言的语法本质上一致的。唯一的区别是locate表的右边是寄存器而不是Locs(寄存器或frame变量)。

	Program	->	(letrec ([label (lambda () Body)]*) Body)
	Body	->	(locate ([uvar reg]*) Tail)
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
	Var		->	uvar | Loc
	Triv	-> Var | int | label

## 5. 样板和运行时代码

都没有变化

## 6. 测试

test4.ss中有这次作业的一个小的测试集，包括有效的和无效的。你需要确保你的编译器至少可以通过测试集中的测试。

## 7. 编码提示

下面是一个简单提示：

* 如果你破坏性地添加一项冲突到冲突表中，即，在找到适合的关联链之后使用set-cdr!，辅助函数uncover-register-conflict只需要返回一个值，live变量集合。不需要返回一个表达式，因为代码内容不改变(唯一改变的是register-conflict表)。

* assign-registers不需要放到代码的body中；它只需要用到locals链表和register-conflict表，并且它不改变任何代码，除了包装locate表和去掉其它包装。

* 尽管discard-call-live需要放到Tail表达式，它并不需要放到Effect和Pred表达式中。