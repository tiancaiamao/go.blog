中文的go语言内部细节的资料几乎没有,所以自己研究了一下

声明：本文内容主要来自本人对源代码的研究,以及网上找到的一些资料的整理,不保证完全正确性

## 函数调用协议

go语言中使用的是非连续栈。原因是需要支持goroutine。

假设调用 go func(1,2,3) ，func函数会在一个新的go线程中运行，显然新的goroutine不能和当前go线程用同一个栈，否则会相互覆盖。

所以对go关键字的调用协议与普通函数调用是不同的。不像常规的C语言调用是push参数后直接call func，上面代码汇编之后会是：

参数进栈

push func

push 12

call runtime.newproc

pop

pop

12是参数占用的大小。在runtime.newproc中，会新建一个栈空间，将栈参数的12个字节拷贝到新栈空间并让栈指针指向参数。

这时的线程状态有点像当被调度器剥夺CPU后一样，pc,sp会被存到类型于类似于进程控制块的一个结构体struct G内。func被存放在了struct G的entry域，后面进行调度时调度器会让goroutine从func开始执行。

defer关键字调用过程类似于go，不同的是call的是runtime.deferproc

函数返回时，如果其中包含了defer语句，不是调用add xx SP, return

而是call runtime.deferreturn，add 48 sp，return

多值返回还没研究明白是怎么实现，如果没记错，C语言中返回值好像是放在eax的，这个估计要放栈里了。有待考证。

## 编译过程分析

$GOROOT/src/cmd/gc目录，这里gc不是垃圾回收的意思，而是 go compiler

6g/8g的源文件的主函数是在lex.c

从这个文件可以看到整个编译的流程。先是利用bison做了词法分析yyparse()

后面就是语法分析,注释中有第一步第二步…最后生成目标文件.8或.6,相当于c的.o

go.y是bison的语法定义文件

事实上go在编译阶段也只是将所有的内容按语法分析的结果放入NodeList这个数据结构里，然后export写成一个*.8(比如i386的架构），这个.8的文件大概是这样子的：

go object linux 386 go1 X:none exports automatically generated from hello.go in package "even"

$$ // exports package even import runtime "runtime" type @"".T struct { @"".id int } func (@"".this *@"".T "noescape") Id() (? int) { return @"".this.@"".id } func @"".Even(@"".i int) (? bool) { return @"".i % 2 == 0 } func @"".odd(@"".i int) (? bool) { return @"".i % 2 == 1 } $$ // local types

$$

可以自己做实验写个hello.go，运行go tool 8g hello.go

具体的文件格式，可以参考src/cmd/gc/obj.c里的dumpobj函数的实现

而如果我们在源文件里写一个import时，它实际上会将这个obj文件导入到当前的词法分析过程中来，比如

import xxx

它就是会把pkg/amd64-linux/xxx.a加载进来，接着解析这个obj文件

如果我们看go.y的语法分析定义，就会看到许多hidden和there命名的定义，比如importthere, hiddenimport等等，这些其实就是从obj文件来的定义。

又比如我们可能会看到一些根本就不存在于源代码中的语法定义，但是它确实编译过了，这是因为在编译过程中源文件被根据需要插入一些其他的碎片进来，比如builtin的一些库或者自定义的一些lib库。

理解了这些，基本上就对go的编译过程有了一个了解，事实上go的编译过程做的事情也就是把它变成obj完事，至少我们目前没有看到更多的工作。接下来想要更深入的理解，就要再看xl的实现了，这部分是将obj变成可执行代码的过程，应该会比较有趣了。

## runtime中的调度器相关

$GOROOT/src/pkg/runtime目录很重要，值得好好研究，源代码可以从runtime.h开始读起。

goroutine实现的是自己的一套线程系统，语言级的支持，与pthread或系统级的线程无关。

一些重要的结构体定义在runtime.h中。两个重要的结构体是G和M

结构体G名字应该是goroutine的缩写，相当于操作系统中的进程控制块，在这里就是线程的控制结构，是对线程的抽象。

其中包括

goid //线程ID

status//线程状态，如Gidle,Grunnable,Grunning,Gsyscall,Gwaiting,Gdead等

有个常驻的寄存器extern register G* g被使用，这个是当前线程的线程控制块指针。amd64中这个寄存器是使用R15，在x86中使用0(GS) 分段寄存器

结构体M名字应该是machine的缩写。是对机器的抽象，这里是可用的cpu核心。

proc.c中是实现的线程调度相关。

如果有自己写过操作系统的经验，看这个会比较过瘾

调度器调度的时机是某线程进入系统调用，或申请内存，或由于等待管道而堵塞等

## 系统的初始化

proc.c中有一段注释

// The bootstrap sequence is: // // call osinit // call schedinit // make & queue new G // call runtime·mstart // // The new G calls runtime·main.

这个可以在$GOROOT/src/pkg/runtime/asm386.S中看到。go编译生成的程序应该是从这个文件开始执行的。

// saved argc, argv … CALL runtime·args(SB) CALL runtime·osinit(SB) //这个设置cpu核心数量 CALL runtime·schedinit(SB)

// create a new goroutine to start program PUSHL $runtime·main(SB) // entry PUSHL $0 // arg size CALL runtime·newproc(SB) POPL AX POPL AX

// start this M CALL runtime·mstart(SB)

还记得前面讲的go线程的调用协议么？先push参数，再push被调函数和参数字节数，接着调用runtime.newproc

所以这里其实就是新开个线程执行runtime.main

runtime.newproc会把runtime.main放到就绪线程队列里面。

本线程继续执行runtime.mstart，m意思是machine。runtime.mstart会调用到schedule

schedule函数绝不返回，它会根据当前线程队列中线程状态挑选一个来运行。

然后就调度到了runtime.main函数中来，runtime.main会调用用户的main函数，即main.main从此进入用户代码

总结一下函数调用流程就是

runtime.osinit –> runtime.schedinit –> runtime.newproc –> runtime.mstart –> schedule –>

runtime.main –> main.main

这个可以写个helloworld了用gdb调试，一步一步的跟

## interface的实现

假设我们把类型分为具体类型和接口类型。

具体类型例如type myint int32 或type mytype struct {…}

接口类型是例如type I interface {}

接口类型的值，在内存中的存放形式是两个域，一个指向真实数据(具体类型的数据)的指针，一个itab指针。

具体见$GOROOT/src/pkg/reflect/value.go 的type nonEmptyInterface struct {…} 定义

itab中包含了数据（具体类型的）的类型描述符信息和一个方法表

方法表就类似于C++中的对象的虚函数表，上面存的全是函数指针。

方法表是在接口值在初始化的时候动态生成的。具体的说：

对每个具体类型，都会生成一个类型描述结构，这个类型描述结构包含了这个类型的方法列表

对接口类型，同样也生成一个类型描述结构，这个类型描述结构包含了接口的方法列表

接口值被初始化的时候，利用具体类型的方法表来动态生成接口值的方法表。

比如说var i I = mytype的过程就是:

构造一个接口类型I的值，值的第一个域是一个指针，指向mytype数据的一个副本。注意是副本而不是mytype数据本身，因为如果不这样的话改变了mytype的值，i的值也被改变。

值的第二个域是指向一个动态构造出来的itab，itab的类型描述符域是存mytype的类型描述符，itab的方法表域是将mytype的类型描述符的方法表的对应函数指针拷贝过来。构造itab的代码在$ROOT/src/pkg/runtime/iface.c中的函数

static Itab* itab(InterfaceType *inter, Type *type, int32 canfail)

这里还有个小细节是类型描述符的方法表是按方法名排序过的，这样itab的动态构建过程更快一些，复杂度就是O(接口类型方法表长度+具体类型方法表长度)

可能有人有过疑问：编译器怎么知道某个类型是否实现了某个接口呢？这里正好解决了这个疑问：

在var i I = mytype 的过程中，如果发现mytype的类型描述符中的方法表跟接口I的类型描述符中的方法表对不上，这个初始化过程就会出错，提示说mytype没有实现接口中的某某方法。

再暴一个细节，所有的方法，在编译过程中都被转换成了函数

比如说 func (s *mytype) Get()会被变成func Get(s *mytype)。

接口值进行方法调用的时候，会找到itab中的方法表的某个函数指针，其第一个参数传的正是这个接口值的第一个域，即指向具体类型数据的指针。

在具体实现上面还有一些优化过程，比如接口值的真实数据指针那个域，如果真实数据大小是32位，就不用存指针了，直接存数据本身。再有就是对类接口类型interface{}，其itab中是不需要方法表的，所以这里不是itab而直接是一个指向真实数据的类型描述结构的指针。

-------------

收集的一些关于go internals的链接：
http://code.google.com/p/try-catch-finally/wiki/GoInternals

http://research.swtch.com/gopackage

http://research.swtch.com/interfaces

http://research.swtch.com/goabstract

http://blog.csdn.net/hopingwhite/article/details/5782888

2013-02-26更新

专门做了个go-internals的深入学习研究
https://github.com/tiancaiamao/go-internals
