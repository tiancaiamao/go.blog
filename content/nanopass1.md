# 作业1

### 内容

1. 背景
2. 开始的Scheme子集
3. 样板代码
4. 运行时系统
5. 汇编指令
6. 编码提示
7. 测试

## 1.背景

这门课中，我们将写一个Scheme子集的编译器，它能够编译生成x86\_64架构下的汇编代码。事实上，我们这个学期将每个星期做一点，包括这个星期，但是过程中Scheme的子集将慢慢变大。编译器也会变得越来越复杂，为了生成更高效的代码。

这个星期的作业是实现一个第2部分所描述的Scheme子集的编译器。这个编译器需要包含下面两个步骤：

* 检验
* 代码生成

检验步骤接受任意的Scheme值作为参数，但是如果这个值不符合当前的Scheme子集，会报错（要有错误描述信息）。代码生成步骤接受一个符合当前Scheme子集的程序，生成对应的x86\_64代码。要将生成的代码打印到当前标准输出端口。

检验步骤应该被命名为verify-scheme，代码生成步骤应该被命名为generate-x86-64。你要能够在一个Scheme程序中运行它们，使用下面这样的简单驱动代码：

	(define driver
		(lambda (program)
			(with-output-to-file "t.s"
				(lambda ()
					(generate-x86-64 (verify-scheme program))))))

这样会输出生成到文件t.s。

第3部分中给出生成的代码的结构。第4部分中给出简单的C代码的运行时，它会调用生成的Scheme程序并打印输出，第5部分描述如何使用gcc汇编生成代码，编译运行时，将两者链接起来，并运行最后的程序。第6部分和第7部分给了一些编码提示和编译器测试指令。

我们所需要知道的X86\_64架构的指令集可以在《X86\_64 Primer》中找到。

## 2.开始的Scheme子集

下面是我们需要处理的最初的Scheme子集：

	Program	->	(begin Statement+)
	Statement	->	(set! Var1 int64)
			|	(set! Var1 Var2)
			|	(set! Var1 (Binop Var1 int32))
			|	(set! Var1 (Binop Var1 Var2))
	Var		->	rax | rcx | rdx | rbx | rbp | rsi | rdi
			|	r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
	Binop	->	+ | - | *

最后两个Statement表中，set!的等式左边的变量和二元操作中的第一个必须是相同的变量，比如，(set! rdx (+ rdx r11))可以，但是(set! rdx (+ r11 rdx))不行。

int32和int64分别是32位和64位精确整数，即，`-2^31<=int32<=2^31-1和-2^63<=int64<=2^63-1`。

操作符号，以及变量名的选择，这些是因为x86\_64架构指令集的限制。后面我们将放松这些限制。

## 3.样板代码

生成的代码必须附上一层样板代码包装，这样C的运行时才可以运行生成的代码，以及返回到C中。样板代码如下，其中generated code表示由生成代码填充。

	.global _scheme_entry
	_scheme_entry:
		generated code
		ret

生成代码必须将rax设置成最终的计算结果。例如，下面代码：

	(begin
		(set! rax 8)
		(set! rcx 3)
		(set! rax (- rax rcx)))

会生成下面的汇编代码：

	.global _scheme_entry
	_scheme_entry:
		movq $8, %rax
		movq $3, %rcx
		subq %rcx, %rax
		ret

这会返回5，并且最终rax寄存器里的值为5。

## 4.运行时系统

运行时系统包含一个main函数和一个打印，目前都很简单。

```
	#include <stdlib.h>
	#include <stdio.h>
	
	#ifdef __APPLE__ /* MacOS */
	#define SCHEME_ENTRY scheme_entry
	#else
	#define SCHEME_ENTRY _scheme_entry
	#endif
	
	extern long scheme_entry(void);
	
	void
	print(long x) {
	  printf("%ld\n", x);
	}
	
	int
	main(int argc, char *argv[]) {
	  if (argc != 1) {
	    fprintf(stderr, "usage: %s\n", argv[0]);
	    exit(1);
	  }
	  
	  print(scheme_entry());
	  return 0;
	}
```

`scheme_entry`需要特殊处理是因为MacOS会给`scheme_entry`加上一个下划线，而我们不想它开头带两个下划线。

将运行时代码放到文件runtime.c中。你也可以选择一个不同的文件名，那么用C编译器编译(如下所示)的时候也要相应做修改。

## 5.汇编指令

为了编译和运行生成的汇编代码，你必须运行在一个64位的Linux或者MaxOS系统中。

生成的汇编代码必须再生成机器码，这样处理器可以运行它。类似地，C的运行时代码也必须编译为机器码。两者需要链接成为一个可执行文件。这三步可以调用GNU C编译器一步完成，它知道如何处理汇编文件(.s文件扩展名的)，也知道如何处理C文件。假设生成的汇编代码像第3部分描述的那样是t.s，并且运行时系统的文件是runtime.c，输入下面命令可以完成这项工作。

	gcc -o t t.s runtime.c

然后运行生成的文件：

	./t

## 6.编码提示

你可以使用像Statement来表示语法结构，我们推荐在文件头部中加入下列代码将case-sensitive设置为true：

	(case-sensitive #t)

如果你使用Chez Scheme的format和printf过程，generate-x86-64写起来将更轻松一些。你现在大概可以定义一些辅助函数来做format操作，生成指令，生成标签等。

尽管这些步骤用if和cond写起来也足够短，如果你用match，代码会更加清晰也更加健壮。match是在match.ss文件中定义的。

如果你使用优化级别2，你的编译器运行起来会快一点，这样你得到反馈也更快一点，将下面代码加到文件头部中：

	(optimize-level 2)

这个告诉系统它可以假设像cons这样的primitive名称是不会被重定义的。坏的一面是不能再重定义primitive的名字了。

## 7.测试

在test.ss里面有一个短小的测试程序，包括有效的和无效的程序。你需要确定你的编译器步骤至少会通过这一系列的测试。
