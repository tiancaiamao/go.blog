# 作业6

### 内容

1. 背景
2. 调用约定
3. Scheme子集6
4. 语义
5. 要做的事
	* 5.1. verify-scheme
	* 5.2. remove-complex-opera*
	* 5.3. flatten-set!
	* 5.4. impose-calling-conventions
	* 5.5. expose-frame-variable
6. 样板和运行时代码
7. 测试
8. 编码提示

## 1. 背景

既然现在编译器可以为我们的局部变量分配寄存器和frame位置了，现在我们要将处理参数和返回值的工作也移交给它。这样在我们的测试用例中就可以不用明确地处理寄存器和frame变量了。我们还会将我们的语言推广到允许procedure和primitive调用中任意层次的表达式嵌套。

为了处理参数，编译器将代码重写，这样形式参数被分配到一系列特定的寄存器和frame位置中，并且在调用时，将实际参数的值写到同样的位置。在返回时，编译器安排将返回值存储到特定的返回值寄存器中，并且跳转(尾调用)到存储在特定的返回地址寄存器中的返回地址。为参数，返回值，和返回地址指定的寄存器，frame位置是由调用协议决定的，它还决定了被调函数要保存的寄存器集合("callee save"寄存器)和调用函数要保存的寄存器集合("caller-save"寄存器)，如果需要的情况下。

## 2. 调用约定

标准的x86\_64调用协议在System V Application Binary Interface AMD64 Architecture Processor Supplement中有描述，不过由于我们的procedure不需要跟其它语言交互，我们将使用我们自己的简单的协议：

* 前n个(n也可能为0)参数将被放到helpers.ss中parameter-registers变量所指定的寄存器中；

* 剩下的参数放到frame位置fv0，fv1等，这些寄存器是映射到由frame-pointer-register指定的基地址的连续位置；

* 由变量return-address-register决定返回地址放到哪个寄存器；

* procedure的返回值是放在哪个寄存器是由return-value-register指定的；

* 所有的寄存器都是调用者保存。

frame指针寄存器可以考虑使用被调者保存，因为当被调者返回时它必须是正好指向调用者的frame上面。整个或者部分的栈可能在程序执行过程中发生重定位，比如，为了支持栈调整大小或者continuation的捕获和调用。因此frame指针寄存器中实际的地址可能会不同，即使它的位置相对于当前frame是固定的。这样，frame指针更适合被当作是被调用者的一个隐式的返回值，并且如果原来的位置如果真的需要保存，则调用者必须保存它。

调用约定总结起来如下图所示。

## 3. Scheme子集6

这周我们处理的Scheme子集的语法添加了形式参数和实际参数，并且不允许明确地指明寄存器和frame位置。它现在允许procedure调用，primitive调用，或者任意嵌套的Value表达式赋值。

	Program	->	(letrec ([label (lambda (uvar*) Body)]* Body)
	Body	->	(locals (uvar*) Tail)
	Tail	->	Triv
			|	(binop Value Value)
			|	(if Pred Tail Tail)
			|	(begin Effect* Tail)
	Pred	->	(true)
			|	(false)
			|	(relop Value Value)
			|	(if Pred Pred Pred)
			|	(begin Effect* Pred)
	Effect	->	(nop)
			|	(set! uvar Value)
			|	(if Pred Effect Effect)
			|	(begin Effect* Effect)
	Value	->	Triv
			|	(binop Value Value)
			|	(if Pred Value Value)
			|	(begin Effect* Value)
	Triv	->	uvar | int | label

唯一变量(uvar)，标签(label)，整数(int)，二元操作(binop)，和关系操作(relop)跟上一个子集相比没有变化。对于整数的机器限制仍然跟上一个子集一样。

## 4. 语义

procedure或者primitive调用的子表达式可以按任意顺序执行，即，从左到右，从右到左，或者任意其它的，只要任意两个参数的计算是不相关的。只有涉及到effect的时候相关性才是可检测的。

## 5. 要做的事

为了处理新的源语言，我们需要更新verify-scheme；添加下面三个新的步骤：

* remove-complex-opera*
* flatten-set!
* impose-calling-conventions

并且对expose-frame-variable做少量的修改。

### 5.1. verify-scheme

这个步骤必须修改以适应语法变化。

### 5.2. remove-complex-opera*

这个步骤去掉procedure和其它primitive调用中嵌套的primitive调用，使参数值变得简单。这步过程生成的语言的语法法描述如下。

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
			|	(set! uvar Value)
			|	(if Pred Effect Effect)
			|	(begin Effect* Effect)
	Value	->	Triv
			|	(binop Triv Triv)
			|	(if Pred Value Value)
			|	(begin Effect* Value)
	Triv	->	uvar | int | label

跟之前的语法相比唯一的变化是primitive调用和procedure调用现在是Triv表达式而不是Value表达式。

为了执行这个，每个Value必须在调用外面赋值给一个新的唯一变量。例如：

	(f$1 (+ (* x.2 x.5) 7) (sra x.1 3))

变成

	(begin
		(set! tmp.7 (* x.2 x.5))
		(set! tmp.6 (+ tmp.7 7))
		(set! tmp.8 (sra x.1 3))
		(f$1 tmp.6 tmp.8))

这个处理过程中引入的新的唯一变量必须要加到Body部分的locals链表中。

## 5.3. flatten-set!

这个步骤重写set!表达式，将它们放到if和begin表达式里面，这样，在输出中，set!的右边即不包含if也不包含begin表达式。我们之所以这样做是为了将赋值转化为更接近汇编形式的表。这一步生成的程序语言的语法描述见下面，唯一的不同是set!的右边只能是Triv或者primitive调用。

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
			|	(if Pred Effect Effect)
			|	(begin Effect* Effect)
	Triv	->	uvar | int | label

当set!表达式右边是一个begin表达式时，set!应该放到begin里面并且为begin内的最后一个子表达式。

	(set! x (begin e1 ... en-1 en)) -> (begin e1 ... en-1 (set! x en))

当右边是if表达式时，set!应该要放到if内部并替换，这样if的then和else部分都变成赋值表达式。

	(set! x (if e1 e2 e3)) -> (if e1 (set! x e2) (set! x e3))

当然，两种情况下右边新的表达式中都有可能是if或者begin表达式，所以需要递归处理直到右边即不是if或者begin表达式。

### 5.4. impose-calling-conventions

这个步骤在添加代码上添加调用转换。它根据需要，将每个要传入的参数安排放到寄存器中或者栈中，并将返回值放到寄存器中。完成之后，lambda表达式不再有确定的形式参数，并且调用和返回都简化成了等价的跳转。这个步骤生成的程序跟uncover-frame-conflict的输入语言是一样的。语法描述如下。

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

我们选择的特定的转换协议不应该硬编码到这步骤的代码中。取而代之，这步应该只假定固定数量(也许为0)的参数寄存器参数p0,p1,...,pn-1，一个frame指针寄存器fp，一个返回值寄存器rv，和一个返回地址寄存器ra。实际使用的寄存器由新的helpers.ss中的变量parameter-registers，frame-pointer-register，return-value-register和return-address-register给出。

应该将这些变量进行不同的设置之后，对这个步骤和剩下的编译器步骤进行测试，包括更多，更少，不同的参数寄存器，不同的返回值寄存器，不同的frame指针寄存器，和不同的返回地址寄存器。测试返回值寄存器跟参数寄存器之一使用同一个也是一个好主意。这步还应该测试只使用很少的寄存器，可以通过将helpers.ss中的变量registers设置成短一点的链表实现。我们推荐你做测试时不要修改helpers.ss，而是将这些变量分配到driver文件中。

这个步骤会执行三次转换来完成。首先，它将lambda表达式中的每个形式参数转化到局部变量中，并且将这些在合适的寄存器和frame位置初始化这些局部变量。

	(lambda (x0 ... xn-1 xn ... xn+m-1)
		(locals (local ...)
			body))
	=>	(lambda ()
			(locals (local ... rp x0 ... xn-1 xn ... xn+m-1)
				(begin
					(set! rp ra)
					(set! x0 p0)
					...
					(set! xn-1 pn-1)
					(set! xn fv0)
					...
					(set! xn+m-1 fvm-1)
					body)))

其中rp是新的唯一变量名用于表示返回地址参数。

它将先为返回地址寄存器和参数寄存器进行赋值。

对于一个letrec的body部分，转换过程是一个重新生成lambda表达式的过程，因为letrec的body中没有形式参数。但是，仍然会有一个隐含的返回地址参数。

	(locals (local ...) body)
	=>	(locals (local ... rp)
			(begin 
				(set! rp ra)
				body))

用于实现lambda的body的Body辅助函数，也可以用于letrec的body，如果它接受空的实参。

然后，它为每次调用中实际参数的值分配合适的寄存器或者frame位置。它将每次调用中的参数的符号替换为一个位置集合，即，返回地址寄存器ra，frame指针寄存器fp，以及参数将要放入的位置的集合。这些位置在调用期间将被假定为live的。集合中位置的顺序是无关的，因为它只是声明了一个调用期间live的位置的集合。

	(proc e0 ... en-1 en ... en+k-1)
	=>	(begin
			(set! fv0 en)
			...
			(set! fvk-1 en+k-1)
			(set! p0 e0)
			...
			(set! pn-1 en-1)
			(set! ra rp)
			(proc fp ra p0 ... pn-1 fv0 ... fvk-1))

这里，rp跟lambda表达式中选用的是同一个唯一变量。

我们最后分配参数寄存器来限制它们的生命期。

处理非尾调用需要一些更多的工作，但是由于我们的子集暂时还没有非尾递归调用，所以我们还不必担忧。

第三步，也是最后，这步将每个Triv或者primitive调用 tail转换成赋值到一个返回值寄存器以及对返回点的调用。当执行调用时，返回值寄存器被假定为live的，因此在调用的时间点它被放到live链表中。返回地址寄存器不应该被包含进来，因为对它的最后一次引用是在调用自身中的。

	expr
		=>	(begin
				(set! rv expr)
				(rp fp rv))

这里也是，rp跟lambda表达式里面的是同一个唯一变量。

如果Tail表达式是一个if表达式或者begin表达式，这一步应该递归直到找到一个Tail表达式要么是一个procedure调用(这种情况是应用了上面第二步变换)要么是一个Triv或者primitive调用（这种情况是应用了第三步变换）。

### 5.5 expose-frame-variable

这个步骤需要更新来，从新的helpers.ss中的变量frame-pointer-register决定实际的frame指针寄存器。它还应该使用helpers.ss中的align-shift变量来决定frame下标移动的字节字，从而避免将机器字长写死到编译器中。

## 6. 样板和运行时代码

样板代码没有变化，但是frame指针，返回地址，返回值寄存器应该由helpers.ss中的变量frame-pointer-register，return-address-register和return-value-register来决定。为此，在helpers.ss中的emit-program已经被重写过了。

运行时代码没有变化。

## 7. 测试

在tests6.ss里面有一个小的测试集，包括会通过的和不通过的测试。你需要确保你的编译器步骤至少能够通过这个测试集。

## 8. 编码提示

在开始之前，研究下在线编译器对于一些样例的输出。

在三个步骤中都要使用make-begin以避免嵌套的begin表达式。