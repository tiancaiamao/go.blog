最近试着优化掉一些小对象分配，发现一个很诡异的问题：这段代码会在堆上分配对象！

    package main

    import (
        "fmt"
    )

    func main() {
        var a [1]int
        c := a[:]
        fmt.Println(c)
    }

看汇编代码

    go tool compile -S test.golang

    "".main t=1 size=336 value=0 args=0x0 locals=0x98
        0x0000 00000 (test.go:7)	TEXT	"".main(SB), $152-0
        0x0000 00000 (test.go:7)	MOVQ	(TLS), CX
        0x0009 00009 (test.go:7)	LEAQ	-24(SP), AX
        0x000e 00014 (test.go:7)	CMPQ	AX, 16(CX)
        0x0012 00018 (test.go:7)	JLS	320
        0x0018 00024 (test.go:7)	SUBQ	$152, SP
        0x001f 00031 (test.go:7)	FUNCDATA	$0, gclocals·7d2d5fca80364273fb07d5820a76fef4(SB)
        0x001f 00031 (test.go:7)	FUNCDATA	$1, gclocals·6e96661712a005168eba4ed6774db961(SB)
        0x001f 00031 (test.go:8)	LEAQ	type.[1]int(SB), BX
        0x0026 00038 (test.go:8)	MOVQ	BX, (SP)
        0x002a 00042 (test.go:8)	PCDATA	$0, $0
        0x002a 00042 (test.go:8)	CALL	runtime.newobject(SB)
        0x002f 00047 (test.go:8)	MOVQ	8(SP), AX
        0x0034 00052 (test.go:9)	CMPQ	AX, $0
        0x0038 00056 (test.go:9)	JEQ	$1, 313
        0x003e 00062 (test.go:9)	MOVQ	$1, DX
        0x0045 00069 (test.go:9)	MOVQ	$1, CX

注意到有调用newobject！其中test.go:8说明变量a的内存是在堆上分配的!

在[Go的FAQ](https://golang.org/doc/faq)里面有这么一段解释：

>    How do I know whether a variable is allocated on the heap or the stack?

>    From a correctness standpoint, you don't need to know. Each variable in Go exists as long as there are references to it. The storage location chosen by the implementation is irrelevant to the semantics of the language.

>    The storage location does have an effect on writing efficient programs. When possible, the Go compilers will allocate variables that are local to a function in that function's stack frame. However, if the compiler cannot prove that the variable is not referenced after the function returns, then the compiler must allocate the variable on the garbage-collected heap to avoid dangling pointer errors. Also, if a local variable is very large, it might make more sense to store it on the heap rather than the stack.

>    In the current compilers, if a variable has its address taken, that variable is a candidate for allocation on the heap. However, a basic escape analysis recognizes some cases when such variables will not live past the return from the function and can reside on the stack.

它说从正确性的角度，用户不用关心内存在哪里分配的就是了。一般来说，如果有地方用到了那个地址，那么变量就会在堆上分配了。比如C中不能，但Go中可以这么干：

    type struct T { xxx}
    func f() *T {
        var ret T
        return &ret
    }

变量ret的内存会在堆上分配的，Go的编译器会决定在哪(堆or栈)分配内存，保证程序的正确性。

Go的编译器很聪明(自作聪明)，它还会做逃逸分析(escape analysis)，如果它发现变量的作用域没有跑出太远，它就可以在栈上分配空间而不是堆。比如这段代码，就不会在堆上分配内存，即使我们用new分配。

    const Width, Height = 640, 480
    type Cursor struct {
        X, Y int
    }

    func Center(c *Cursor) {
        c.X += Width / 2
        c.Y += Height / 2
    }

    func CenterCursor() {
        c := new(Cursor)
        Center(c)
        fmt.Println(c.X, c.Y)
    }

验证一下:

    go tool compile -m test.go

    test.go:17: can inline Center
    test.go:24: inlining call to Center
    test.go:25: c.X escapes to heap
    test.go:25: c.Y escapes to heap
    test.go:23: CenterCursor new(Cursor) does not escape
    test.go:25: CenterCursor ... argument does not escape
    test.go:17: Center c does not escape

参数-m是打印出编译优化。从输出上看，它说new(Cursor)没有escape，于是在栈上分配了。等价于C的写法：

    void CenterCursor() {
        struct Cursor c;
        Center(&c);
    }

再看另一个代码，跟开始那段代码的区别，一个用的fmt.Println，一个用的println。

    package main

    func main() {
        var a [1]int
        c := a[:]
        println(c)
    }

但是!!! 这段代码a是在栈上分配的。而最上面那段，却是在堆上分配的。

综上，Go一方面会把一些，看上去会在栈上分配的东西，移到堆上分配；另一方面又会把看上去会在堆上分配的东西，在栈上分配。由编译优化那边做逃逸分析来控制。

这优化太诡异了，最后实在没法直观地知道，哪些会在堆上分配，哪些会在栈上分配。老实说，我**非常**不喜欢这个feature。Go自做主张弄一些背后我无法确定的事情。顺带一提吧，另一个Go中我不喜欢的feature是init函数。
