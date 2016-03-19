## 介绍

plan9是一个很先进的操作系统，没能流行实在太可惜了。Go算是继承了plan9衣钵并广为人知的。像[acme](acme.md)就没这么好运了，这么好的东西居然没人发现。今天我又从plan9port中扒了一个好东西出来，那就是libthread。

libthread是什么呢？它是一个协程的库，简单地说就是这个库让你能够在C中使用goroutine和channel。其实早在Go之前，这东西就存在了，应该说是Go语言它爹了。好东西总是要发光的，就算以前没发光，Go语言同样技术换个名字再放出去，就发光了。我重新整理了一下代码，开了个项目，名称是grt，意思go runtime，学着crt取的名字。

下载地址：[https://github.com/tiancaiamao/grt](https://github.com/tiancaiamao/grt)

## 使用场景

考虑可能的使用场景，比如项目不想转到Go，或者还是想手动管理内存，又想使用goroutine那样的基础设施，那么grt就值得考虑了。

再比如说，有些遗留代码，用Go重写工作量很多，而用cgo又担心性能问题，那么迁移到grt就很合适。

## API

这个库实现了procs，threads，channels和locks。

	int		proccreate(void (*f)(void *arg), void *arg, unsigned int stacksize);

创建一个proc，proc是一个操作系统线程。

	int		threadcreate(void (*f)(void *arg), void *arg, unsigned int stacksize);

创建一个thread，这里的thread不是操作系统线程而是类似goroutine的东西。所有的thread会在procs之运行。

	Channel* chancreate(int elemsize, int elemcount);

chancreate创建一个带缓存或者不带缓存的channel。element无类型，但是每个element大小必须相同。

	int send(Channel *c, void *v);
	int recv(Channel *c, void *v);

send将指针v指向的内容写到channel c中。send会阻塞直到channel中有空间，或者有其它thread向channel中读取。类似地，recv向channel中读一个元素。如果channel暂时不可读则recv会阻塞。

	typedef struct Alt {
	Channel *c; /* channel */
	void *v; /* pointer to value */
	Altop op; /* operation */
	Channel **;
	ulong;
	} Alt;
	int alt(Alt alts[]);

Alt就是Go语言中的select操作。

	int f(Channel *c0, Channel *c1) {
		int v0; char v1;
		Alt a[] = {
			/* c v op */
			{c0, &v0, CHANRCV},
			{c1, &v1, CHANRCV},
			{nil, nil, CHANEND},
		};
		for (;;)
			switch(alt(a)) {
				case 0:
					fprint(2, "got int %d\n", v0);
					break;
				case 1:
					fprint(2, "got char %c\n", v1);
					break;
				default: error("impossible");
			}
	}

## 原理

其实原理不复杂，我以前也[写过类似](task.html)的玩意。

主要涉及的就是保存上下文，封装非阻塞io，实现调度，然后channel提供coroutine之间通信。

保存上下文可以用getcontext做，不过getcontext是系统调用，会陷入内核，性能不太好。grt的保存上下文是用汇编写的。

调度跟早期版本的Go很像，proc对应于Go的结构体M，thread对应于结构体G。每个proc执行thread的函数，遇到阻塞时切换thread。

复杂一点的地方是线程和协和实现m:n调度。另外，对于signal处理也是复杂一些的。应该说，unix的signal跟posix的pthread起源本来就不是同一套，本身就是比较黑暗的。

## 相似项目

[Go语言](http://golang.org/)，熟习Go语言一眼就能理解grt了。本来libthread是Go的前身。

[libtask](http://swtch.com/libtask/)，提供了几乎同样的接口。事实上作者是同一人，不过libtask是单线程的。而grt是多线程的。libtask的代码更简单一些，有很大的学习价值，但grt更实用一些。

[skynet](https://github.com/cloudwu/skynet)，也提供了类似的基础设施。不过skynet是Actor模型，而grt准确来说是CSP模型。另外，skynet在C的层面并没有提供协程机制，只是一个消息分发和调度，保存上下文是通过lua层面实现的，而grt是纯C。

## 结语

最后说一句，这东西不是我写的，我只是从[plan9port](http://swtch.com/plan9port/)中扒的代码。

----------2016.3.19更新----------

grt被我用来弄另一个项目名字了，内容已经完全变了。如果想看看libthread，可以自去plan9port扒一扒代码。
