# 序

nanopass是一种新颖的写编译器的方式，最早听到nanopass这个词是在王垠的文章《Chez Scheme 的传说》里，这里先引用大段的原文，省得自己费力地解释一遍还说明白：

	Kent 的课程编译器有很好的结构，它被叫做“nanopass 编译器构架”。它的每一个 pass 只做很小的一件事情，然后这些 pass 被串联起来，形成一个完整的编译器。编译的过程，就是将输入程序经过一系列的变换之后，转化为机器代码。你也许发现了，这在本质上跟 LLVM 的构架是一样的。但是我可以告诉你，我们的课程编译器比 LLVM 干净利落许多，处于远远领先的地位。每一节课，我们都学会一个 pass。每一个讲义，都非常精确的告诉你需要干什么。每一次的作业，提交的时候都会经过上百个测试（当然 Kent 不可能把 Chez Scheme 的测试都给我们），如果没有通过就会被拒绝接受。这些测试也可以下载，用于自己的调试。有趣的是，每一次作业我们都需要提交一些自己写的新测试，目的是用于“破坏”别人的编译器。所以我们每次都会想出很刁钻的输入代码，让同学的日子不好过。当然是开玩笑的，这种做法其实大大的提高了我们对编译器测试的理解和兴趣，以及同学之间的友谊。这比起我曾经在 Cornell 选过（然后 drop 掉）的编译器课程，真是天壤之别。

文章里也有提到：

	如果你有兴趣的话，可以看看我最后的代码。由于版权原因，有些辅助部件我不能放在网上，所以你并不能运行它，只能看一个大概的形状。

王垠的东西，很容易会lost in the future的，所以我[fork了一份代码](https://github.com/tiancaiamao/yscheme)，并且在这个基础上整理了一些其它的资料。如王垠所说，他的代码直接拿过来不能运行的。所以我要在学习和研究代码过程中，恢复出一份能运行的版本。

原本这一个学期的课程的，而对我来说，其一水平有限，远没达到王垠那种大神级别；其二，不像垠神那样，能得到Kent，那个“编译器领域No.1”的指导；其三，我甚至没有一个学期那么充足时间专门研究。所以，并不能指望对nanopass能有多么高深的见解。甚至害怕自己粗浅的理解会以讹传讹，但是我还是想把这些东西整理一下，于是决定写这个系列。scheme语言编译器太过于小众，也希望有更多的人能理解。王垠技术很牛，希望大家不要无脑喷，让我们看他的代码而不是言论。

# 环境配置

代码需要petite的运行环境，petite是chez scheme的免费的解释器版本，[这里](http://www.scheme.com/)可以下载。编译安装的就不细说了。推荐使用emacs编辑器，至少比直接在终端下使用petite会方便很多。

将代码clone下来以后，可以看到代码并不多。最主要的就是compiler.ss这个文件。在compiler.ss中可以看到类似这样的代码：

	(define compiler
	  (compiler-generator 
	   '(convert-complex-datum
	     uncover-assigned
	     purify-letrec
	     pre-optimize
	     convert-assignments
	     optimize-direct-call
	     remove-anonymous-lambda
	     sanitize-binding-forms
		 ;  uncover-free
	     convert-closures
	     analyze-closure-size
		;  optimize-known-call
	     introduce-procedure-primitives
	     lift-letrec
	     normalize-context
	     specify-representation
	     uncover-locals
	     remove-let
	     verify-uil
	     remove-complex-opera*
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
	     finalize-locations
	     expose-frame-var
	     expose-basic-blocks
	     optimize-jumps
	     flatten-program
	     analyze-code-size
	     generate-x86-64  ;; turn it on only on 64-bit machines
	     ) 
	   parse-scheme))

里面有很多参数，每个参数都是一个pass，每个pass会处理一个输入，生成一个输出，将这些所有pass组合起来，最终就得到了一个编译器。

# 运行

driver.ss暂时运行不了。可以手动操作，进入到各次课程作业的目录下面运行petite。比如在Assignment-2中，

	(load "../match.ss")
	(load "helpers.ss")
	(load "fmts.pretty")
	(load "a2.ss")

然后手动调用里面各个pass，观察输入和输出。比如

	(generate-x86-64
		'(code
		    (set! rax 17)
		    (jump f$1)
		    f$1
		    (set! #<disp rbp 0> rax)
		    (set! rax (+ rax rax))
		    (set! rax (+ rax #<disp rbp 0>))
		    (jump r15)))

# 内容大纲

repo里面有原始的课程的英文的作业资料，想读的也可以去读一下。

接下来的一系列就是翻译他们的教案了，会从最基本的汇编开始讲起，慢慢进化到高级语言。期间涉及到基于图着色的寄存器分配算法，使用的中间语言UIL等等，慢慢更新吧。

好，开始了...

* [nanopass之一--代码生成](./nanopass1.md)
* [nanopass之二--带括号的汇编语法](./nanopass2.md)
* [nanopass之三--变量和跳转](./nanopass3.md)
* [nanopass之四--寄存器分配](./nanopass4.md)
* [nanopass之五--frame分配和迭代](./nanopass5.md)
* [nanopass之六--处理调用参数和返回值](./nanopass6.md)
* [nanopass之七--非尾调用](./nanopass7.md)
* [nanopass之八--通用中间语言](./nanopass8.md)

未完侍续