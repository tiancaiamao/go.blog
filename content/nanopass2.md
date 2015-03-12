# 作业2

### 内容

1. 背景
2. Scheme子集2
3. 语义
4. 要做的事
	* 4.1. verify-scheme
	* 4.2. expose-frame-var
	* 4.3. flatten-program
	* 4.4. generate-x86-64
5. 样板代码
6. 运行时系统
7. 编码提示
8. 测试
9. 关于frame变量

## 1. 背景

这个学期我们最终的目标是下面语法所描述的Scheme子集的编译器。这个语法子集中，常量包含定长整数(fixnum)，布尔，数据包括定长整数，布尔，空链表，pair和vector。定长整数是范围受限的整数，数值大小范围由我们选择的表示决定。原语(primitive)是Scheme过程*,+,-,<,<=,=,>=,>,add1,sub1,zero?,boolean?,integer?,null?,pair?,procedure?,vector?,not,eq?,cons,car,cdr,set-car!,set-cdr!,make-vector,vector-length,vector-ref,vector-set!和void。

	Expr	->	constant
			|	(quote datum)
			|	var
			|	(set! var Expr)
			|	(if Expr Expr)
			|	(if Expr Expr Expr)
			|	(begin Expr Expr*)
			|	(lambda (var*) Expr Expr*)
			|	(let ([var Expr]*) Expr Expr*)
			|	(letrec ([var Expr]*) Expr Expr*)
			|	(primitive Expr*)
			|	(Expr Expr*)

在作业1中，我们写了一个小得多的Scheme子集的编译器，它也是正好设计成一个小的括号版本的汇编语言。除去源代码验证步骤，
我们的作业1中的编译器由一个代码生成步骤构成，接受括号版本的汇编语言输入，生成格式化的汇编代码。这正是代码生成该做
的事情。代码生成是跟特定机器绑定的，如果我们把太多的东西放在代码生成中，将编译器移植到其它机器我们需要重写太多代码
。因此，尽管我们的代码生成会增长并处理更多的x86\_64汇编代码子集，它还是会保持仅仅是一个汇编代码格式化的角色。由于随着我们的泛化和扩展我们的输入语言，它将会越来越不像汇编，我们需要在源代码验证和代码生成之间，添加额外的步骤来将源代码转化为括号的汇编代码。

这个学期，我们将会扩展编译器能处理的Scheme子集，有时候我们会精益求精，会使代码生成变得更加高效。每个星期，我们会添加新的步骤到编译器中，或者为现有的步骤增加更多工序，或者两都都做。

在这次作业中，我们首先我们编译器处理的源语言，目标是使它更接近上面描述的Scheme子集。这次作业的另一个目标是扩展代码生成使它能处理更大的一个x86\_64汇编语言子集。在某种意义上，这只是次要目标因为如果不需要处理更大的语言我们就不必要扩展现在的代码生成。另一方面看，一旦我们弄好了代码生成，我们可以将工作重心移到更高层的任务。所以扩展源语言的设计也会考虑到要生成接近最终版本的括号形式汇编语言。

## 2. Scheme子集2

下面是这周我们将要处理的Scheme子集的语法。

	Program	->	(letrec	([label (lambda () Tail)]*) Tail)
	Tail	->	(Triv)
			|	(begin Effect* Tail)
	Effect	->	(set! Var Triv)		
			|	(set! Var (Binop Triv Triv))
	Var		->	reg | fvar
	Triv	->	Var | int | label

寄存器reg是一个x86\_64寄存器名称的符号：rcx,rdx,rbx,rbp,rsi,rdi,r8,r9,r10,r11,r12,r13,r14和r15。

frame变量fvar是一个命名形式为fvindex的符号，其中index是非空的数字序列，序列开头中没有不必要的0，比如fv0，fv3，或者fv10。frame变量将原来有限的寄存器变量扩展到一个无限制的变量集合。

标签label是一个命名形式为prefix$suffx的符号，其中suffix是一个非空的，开头不带不必要0的，数字序列，比如，f$0，minimal?$3，destroy-instance!$50，或者$$$$17。每个标签必须有一个唯一的后缀，但是不需要唯一的前缀，因此，f$1和f$2可以用在同一个程序中，但是f$1和g$1不能。每个程序中每个标签应该是对应正好一个letrec绑定。

整数int是精确整数。

二元操作binop是+,-,*,logand,logor和sra中的一个。其中除了sra都是普通的Scheme原语。sra代表右移位运行("shift right arithmetic")，它在定义在helper.ss是给出。

现在的语法表示依赖受限于特定的x86\_64目标架构的限制。尤其是：

* 在(set! Var (op Triv1 Triv2))中，Triv1必须是跟Var一样的。
* 标签不能够作为任何一个二元操作。
* 在(set! Var Triv)中，如果Triv是一个标签，Var必须是一个寄存器。(这是因为我们需要使用leaq指令，它要求目标是一个寄存器)
* 在(set! Var Triv)中，如果Triv是一个整数n，那么必须满足-2^31<=n<=2^31-1或者-2^63<=n<=2^63-1，并且Var必须是一个寄存器
* 对于(set! Var (* Triv Triv))，Var必须是一个寄存器。
* 对于(set! Var (sra Triv1 Triv2))，Triv2必须是一个整数k，并且0<=k<=63。任何其它二元操作中的整数n必须是精确整数，且-2^31<=n<=2^31-1。
* 对于(Triv)，Triv不能够是整数。

我们没有把这些机器限制加到语法中，是因为我们想留下可能性，因为将来其它目标机器上可能会有不同的限制。从长远来看，这些限制只会影响到代码生成，还有指令选择步骤。目前来看，它们还影响到我们的验证步骤和测试代码。

## 3. 语义

lambda表达式在我们新的子集中是用来创建procedure的，但是它们和Scheme的procedure不太一样。一方面，lambda表达式出现在程序的全局定义中，就像C中的函数定义那样。另一方面，这些procedure是不带参数的(尽管他们可以接受寄存器和frame变量)。实际上，这些过程仅仅是带标签的代码块，标签是由letrec表中指定。这跟我们直接在汇编代码中表示是一样的，因此，现在我们跟括号形式的汇编差别不大。

我们新的子集中的调用也是受限的，首先是因为他们都是不带参数的表达式，其次是它们只能出现在尾位置上--这样所有的调用都是尾调用。尾调用有时候也叫带参数的跳转。我们的尾调用是不带参数跳转，即，简单跳转。同样，这跟我们直接用汇编代码表示是一样的。

为了处理frame变量，运行时系统和样板代码要合作将rbp寄存器设置到栈区域，为了允许从Scheme代码中返回到样板代码，寄存器r15("我们的返回地址寄存器")设置为了`_scheme_exit`标签，每个程序最后都要跳转到它从而返回到运行时系统(第5部分)。

一个有趣的问题是，是否这个语言可以执行任意地计算，即，是否图灵完备。尤其是，它是否能够表达无限循环的程序，然后依赖于某些条件改变而终止？尽管有所有调用都是尾调用的限制，它是否能够，以某种方式够表达递归过程？

## 4. 要做的事

为了处理新的源语言，我们需要更新验证步骤，写两个新的步骤将源语言转化为带括号的汇编语言，并更新代码生成步骤。两个新的步骤是expose-frame-var，它将frame变量fvar转换成内存操作，flatten-program，它将代码变成标签和statement。新的以及修改过的步骤在4.1-4.4中描述。

### 4.1. verify-scheme

这个步骤要修改来反映Scheme子集结构的变化，以及set!表的机器限制。

### 4.2. expose-frame-var

expose-frame-var的工作是将出现的frame变量fv0,fv1等，转化为替换模式操作(见X86\_64 Primer)，以rbp寄存器作为基地址，偏移量基于frame变量的index。由于我们的机器字节是64位，即，8字节，fvi的偏移量应该是8i，即，fv0,fv1,fv2对应的分别是0,8,16。

见第9部分对frame和frame变量的描述和它们在内存中的位置。

使用helpers.ss中的make-disp-opnd来构建替换模式操作。这个操作是define-record定义的一个disp-opnd，还有附带的disp-opnd?操作以及成员访问函数disp-opnd-reg和disp-opnd-offset。

expose-frame-var这个步骤中其它的地方不需要改变。

例子：源程序员

	(letrec ([f$1 (lambda ()
					(begin
						(set! fv0 rax)
						(set! rax (+ rax rax))
						(set! rax (+ rax fv0))
						(r15)))])
		(begin
			(set! rax 17)
			(f$1)))

会被转化为

	(letrec ([f$1 (lambda ()
					(begin
						(set! #<disp rbp 0>> rax)
						(set! rax (+ rax rax))
						(set! rax (+ rax #<disp rbp 0>))
						(r15)))])
		(begin
			(set! rax 17)
			(f$1)))

其中 #<disp reg offset> 是替换模式操作的表示。

## 4.3. flatten-program

这个步骤将我们现在嵌套的源语言转化为更类似汇编的形式，没有letrec和begin表，调用变成直接跳转，procedure名称变为标签表。它会生成一个code表，包含一系列的标签，副作用表达式，和跳转。跳转是出现在letrec中的lambda表达式的标签紧接着body部分。

例子：expose-frame-var的输出

	(letrec ([f$1 (lambda ()
					(begin
						(set! #<disp rbp 0> rax)
						(set! rax (+ rax rax))
						(set! rax (+ rax #<disp rbp 0>))
						(r15)))]))

会被转化为如下所示。

	(code
		(set! rax 17)
		(jump f$1)
		f$1
		(set! #<disp rbp 0> rax)
		(set! rax (+ rax rax))
		(set! rax (+ rax #<disp rbp 0>))
		(jump r15))

## 4.4. generate-x86-64

这个步骤必须修改成处理code表而不是之前的begin表，并且要加上处理标签，跳转，以及新加入的logand,logor和sra操作。

例子：flatten-program的输出

	(code
		(set! rax 17)
		(jump f$1)
		f$1
		(set! #<disp rbp 0> rax)
		(set! rax (+ rax rax))
		(set! rax (+ rax #<disp rbp 0>))
		(jump r15))

会被转化为如下所示：

		movq $17, %rax
		jmp L1
	L1:
		movq %rax, 0(%rbp)
		addq %rax, %rax
		addq 0(%rbp), %rax
		jmp *%r15	

## 5. 样板代码

样板代码变得更加复杂了，首先是因为我们要意识到有一些x86\_64寄存器(rbx和r12-r15)需要保存，然后是rbp需要设置为运行时库创建的栈的基地址(通过首个C的寄存器参数rdi)，第三点是因为返回地址必须要放到r15中。

		.global _scheme_entry
	_scheme_entry:
		pushq %rbx
		pushq %rbp
		pushq %r12
		pushq %r13
		pushq %r14
		pushq %r15
		movq %rdi, %rbp
		leaq _scheme_exit(%rip), %r15
		generated code
	_scheme_exit:
		popq %r15
		popq %r14
		popq %r13
		popq %r12
		popq %rbp
		popq %rbx
		ret

## 6. 运行时系统

新的运行时系统在文件runtime.c中。它比上周我们的运行时系统更加复杂了，因为它需要处理为栈创建存储空间，以及传递栈地址参数到样板代码。它还设置了一个堆存储空间并将堆空间地基地址作为第二个参数传到样板代码，不过样板代码中忽略了堆，因为我们暂时还没用到。

## 7. 代码提示

即使你没有在第一次作业中使用match，那么这次你也该用它了。

在我们的helpers.ss中有一系列的辅助函数，包括上面描述过的make-disp-opnd过程，寄存器，frame变量和标签判断的谓词，还有我们的emit宏，大概会很有用。帮助文档都在文件的头部。

## 8. 测试

这次作业的一个小的测试集都在tests2.ss中，包括有效和无效程序。你需要确保你的编译器致可以通过这部分测试集。

寄存器和默认的frame变量(从fv0到fv100)是在helpers.ss中预定义的，方便测试。frame变量是设置成宏，操作表示栈的vector中的元素，基于rbp中的下标。所以，如果rbp是0，那么fv0访问栈中的第一个元素，fv1访问第二个...

## 9. 关于frame变量

我们有15个可用寄存器，这对大部分的procedure都是够用了的。毕竟，有多少procedure是在同一时间内使用超过15个变量的呢？看看你到目前为止写的50多个procedure，很可能，都在15个变量以内。

但是，如果一个procedure需要超过15个变量会发生什么事呢？如果一个procedure调用第二个procedure，第二个调第三个，如此反复，那么整个调用链需要超过15个变量，会发生什么事？如果一个procedure非尾递归地调用它自己，调用深度不确定，会发生什么事情？如果调用一个库的procedure，但是调用方不确定到底这个库会用到多少个寄存器，这里会发生什么事情？

答案是，给予每个procedure一个空的内存块，这块空间被叫做frame，为了支持递归，frame是存储在栈中的。有很多方法要以做，比如说，调用的时候，一个procedure可以通过下移栈地址分配所有它需要的空间，然后在它返回的时候再上移栈地址来释放空间。或者，它可以在需要用的时候分配和回收栈内的空间。不管哪种，在任意某个时间点，procedure执行时所占用的空间都可以被当作是这个procedure的frame。

在我们的编译器中，我们会采用"frame指针"来替代"栈指针"这个词，frame指针总是指向procedure的frame的基地址的。如果procedure执行一个非尾递归调用，它需要确保调用返回的时候，任何需要用到的变量都是在它的frame中的，调整frame指针到保存值的上方，执行调用，最后再在调用返回的时候将frame指针调整回它自己的栈。

后面我们会再谈关于这个过程的。现在，编译器不需要支持非尾递归，所以我们不需要担心调整frame指针。本质上，我们所有需要的只是一个frame，因为通过尾调用，每个procedure调用都可以重用它的调用者的frame。

然而，我们确实需要为frame指针保留一个寄存器，因此这里我们使用机器设计的方式来使用这个寄存器，即rbp寄存器。我们假设运行时提供了我们所需要的单个frame空间，并且样板代码将frame空间的基地址放到了bfp寄存器中。