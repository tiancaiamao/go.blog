无栈协程可以在编译器做些支持，提供 await yield 之类的关键字，其实本质上就是回调写法用 monad 的形式让代码不那么回调地狱。无栈协程使用起来不太舒适，即使语法糖各种已经弄得很方便的情况。有个无解的痛点是，只要是无栈协程实现，都克服不了函数染色的问题。

有栈协程则是必须依赖语言的 runtime 层支持的。所以市面上无栈协程远多于有栈协程的实现。比较著名的无栈协程，只有 Go 是语言级别支持的。然后有一些其它的脚本型语言，比如说 lua 这种，是在虚拟机里面支持的 stackful coroutine。在字节码虚拟机里面去实现有栈协程相对会容易一些，毕竟虚拟机的堆栈等上下文都是受控的。但是相对来说性能会差一点。

有没有原生 C 语言的有栈协程呢？有一些这样的库，比如说云风的 [coroutine](https://blog.codingnow.com/2012/07/c_coroutine.html)，腾讯的 libco，还有我一前同事也写过，顺手帮他打个[广告](https://github.com/hnes/libaco)，但是这类的实现方式我都不太喜欢。都是依赖于 setcontext 或者用一些汇编，很 hack 的形式获取 C 的上下文，替换上下文。这些都是依赖于系统，依赖于硬件平台，依赖于编译器的，虽然你可以说，主流的环境都可以支持，但是我始终不太喜欢这种形式的库。

我认为更合适的方式是把一门高层语言，transpiler 到 C 语言，然后提供协程的支持。同时，这门高层语言里面把垃圾回收之类的，也一并处理掉。于是得到一个很方便的上层开发环境，同时也不过多地失去 C 语言的性能。C 这门语言，不适合手写，更适合当现代的汇编语言玩。我设计的 [cora 语言](https://github.com/tiancaiamao/cora)就是这么干的，它支持了协程，只不过当前 0.3 版本并不是完全 transpiler 的形式，而是把 C 语言这门语言当成了我的虚拟机，有一些中间环节的抽象。为了得到更好的性能，我在思考下一个版本的实现，尽可能地做 transpile，而不仅仅是把语言当虚拟机用，这样损失的性能是更小的。思考中得到了一个好的 idea，本文将描述一种通用的 C 语言有栈协程的实现方式。


首先我们想下 iterator 的概念。一个 iterator 是可以调用多次的，它不是调用一次就返回了，而是可以 next 多次直到最终结果的完成。
我们可以搞一个状态机，记录里面的状态，每次 next 之后更新状态，直到结束。无栈协程的库就是这么干的。以下我们将以 fib 函数的例子来说明。

[v0.c](https://github.com/tiancaiamao/c-coroutine-demo/blob/main/v0.c)

```
int fib(int n) {
	if (n < 2) {
		return n;
	}
	return fib(n-1) + fib(n-2);
}
```

改成状态机的写法，fib 函数会经历 fib(n-1) 的计算，会经历 fib(n-2) 的计算，以及将两者的结果相加后返回。我们可以引入状态机，使得代码可以在每一个阶段停下来：

[v1.c](https://github.com/tiancaiamao/c-coroutine-demo/blob/main/v1.c)

```
struct fibFrame {
	int label;
	int val1;
	int val2;
};

int fib(struct fibFrame *frame, int n) {
 again:
	switch (frame->label) {
	case 0:
		if (n < 2) {
			return n;
		} else {

			struct fibFrame xx = {.label = 0};
			frame->val1 = fib(&xx, n - 1);
			frame->label++;
			goto again;
		}
	case 1:
		{
			struct fibFrame yy = {.label = 0};
			frame->val2 = fib(&yy, n-2);
			frame->label++;
			goto again;
		}

	case 2:
		frame->label++;
		return frame->val1 + frame->val2;
	}
}
```

如果不是 goto again，而是把 fibFrame 返回，这样的状态机写法，我们是可以停下来的，只要把必要的信息都记录在 fibFrame 里面，返回之后下次用户手动调用 fibFrame，再去驱动下一个 case 的执行。变成一步一步地在 fibFrame 做 next()，每次重入后进到下一个 switch 的 case。

有什么好处呢？我们是得到的 fibFrame 其实一个 promise 或者 future，这个东西最终会返回给我们 fib(n) 的执行结果，**这个东西是中途可以停顿下来的**，我们可以手动来控制它的下一步执行。有了这个机制，我们就有了 yield。有了 yield，我们就可以把当前执行的上下文停下来，把控制流切换到另一个，然后在必要的时候再切回来。我们就可以同时执行多个上下文了，也就是可以有协程了。这就是典型的无栈协程的做法。

为什么叫无栈协程？因为**全部上下文信息都是保存在 fibFrame 结构体里面的**，对于 fib 函数用到的状态保存叫 fibFrame，对于其它的函数可能是叫 stateFrame，或者其它任何名字，最通用的叫法，叫 Future<T>。一个一个的 Future<T> 以链式的方式串起来，就是无栈协程了。


注意我们上面的写法中，调用 fib(n-1) 和 fib(n-2) 的时候，是构造了一个 struct fibFrame 的。上面的代码是在 C 语言的栈上面分配的，这只不为了演示，并不正确。如果正常的无栈协程，通常是在堆上面分配这个结构体的内存，这样才能保证比如 fib(n-2) 里面的某个位置可以 yield 出来，下次重入内存不消失。如果这块内存使用的是 C 语言的栈，那是不行的，因为 yield 出去之后，下次重入时这个 fibFrame 在 C 栈上的内存就不可用了，所以**需要堆上分配 fibFrame**。

OK，我们可以改造一下代码，传一个分配器进去，在堆上面分配 fibFrame 的内存：

```
int fib(struct Alloc *alloc, struct fibFrame *frame, int n) {
 again:
	switch (frame->label) {
	case 0:
		if (n < 2) {
			return n;
		} else {

			struct fibFrame *xx = alloc->Alloc(sizeof(*xx));
			frame->val1 = fib(xx, n - 1);
			frame->label++;
			goto again;
		}
	case 1:
		{
			struct fibFrame *yy = alloc->Alloc(sizeof(*yy));
			frame->val2 = fib(&yy, n-2);
			frame->label++;
			goto again;
		}

	case 2:
		frame->label++;
		return frame->val1 + frame->val2;
	}
}
```

这样改造之后，就基本上是一个无栈协程的原型了。

无栈协程和有栈协程，最主要的区别是什么？是栈！无栈协程会有协程栈，每个函数调用是一个帧(frame)，多个连续的帧构成了协程的栈。
再看上面的代码，我们是否真的需要一个 allocator？我们是否可以用一个 arena 分配器，直接连续分配 fibFrame 呢？完全是可以的呀。

[v2.c](https://github.com/tiancaiamao/c-coroutine-demo/blob/main/v2.c)

```
int fib(struct stack *stk, struct fibFrame *frame, int n) {
 again:
	switch (frame->label) {
	case 0:
		if (n < 2) {
			return n;
		} else {
			// 保存 frame label
			// 用于返回到计算 fib(n - 2)
			frame->label++;
			
			// 计算 fib(n-1) ... 调用操作
			struct fibFrame *xx = (struct fibFrame*)(stk->ptr + stk->len);
			stk->len += sizeof(*xx);
			xx->label = 0;
			frame->val1 = fib(stk, xx, n - 1);
			stk->len -= sizeof(*xx);

			goto again;
		}
	case 1:
		// 返回到 + 操作
		frame->label++;

		// 计算 fib(n - 2)
		struct fibFrame *yy = (struct fibFrame*)(stk->ptr + stk->len);
		stk->len += sizeof(*yy);
		yy->label = 0;

		frame->val2 = fib(stk, yy, n - 2);
		stk->len -= sizeof(*yy);
		goto again;

	case 2:
		return frame->val1 + frame->val2;
	}
}
```

这里故意把 fibFrame 的 arena 分配器的命名叫做 stack，因为当每次调用里面 fibFrame 是连续地顺序分配的时候，它本质上就是有栈协程的栈了。
我们完全没有使用 C 语言的栈，而是自己来管理我们协程栈的内存分配。fib 调用和返回，会涉及分配新的栈和回收栈的过程，这里就是 stk 中一个指针的 push 和 pop 而已。

有栈协程的上下文切换，需要把协程的栈保存起来，现在这个操作变得很容易了，毕竟协程栈的内存是我们自己在手动管理，而不是 C 在管理。我们可以实现一个 yield 函数，函数签名跟 fib 差不多，它做的事情就是把 stk 拷贝出来，协程栈就捕获到了。然后我们就可以切换到其它的协程上面去执行。如果需要的时候，再恢复这个保存的协程栈，就可以恢复协程的执行。

这里没有展开讲的是，这里的协程栈的内存管理策略。它可以是分段栈，可以是连续栈，可以是灵活组合的。cora 语言中当前的做法是提供 resumable exception 的概念，对外提供的 API 是 try, throw, catch。在 try 的时候会新开一个 2K 大小的栈，而不是沿用当前执行栈，然后在 throw 的时候，会从当前栈位置一直捕获到 try 的栈的位置为止，包装成 delimited continuation，返回给 catch 的函数。catch 函数可以决定是恢复协程还是其它处理。如果协程栈大小超过 2K 之后，则再分配下一个 2K 链在前面的栈上，也就是用的分段栈策略。

```
(try (lambda () (+ (throw 42) 1))
    (lambda (e k)
        (cont 666)))
```

再回到正题，我们保存上下文已经可以保存栈了。换算成汇编的概念是 sp 和 bp 寄存器已经是自己管理的了。但是上下文管理还需要 pc 寄存器。

如果我们像这样做：

```
			struct fibFrame *xx = (struct fibFrame*)(stk->ptr + stk->len);
			stk->len += sizeof(*xx);
			xx->label = 0;
			frame->val1 = fib(stk, xx, n - 1);
			stk->len -= sizeof(*xx);
```

函数返回地址(保存的 pc 寄存器)其实是存储在 C 语言的栈上的。这种情况下要捕获完整的上下文，需要捕获 C 的栈，这不是我想要的。
为了能捕获 pc 寄存器，我们需要自己来管理调用链，跟自己要管理 fibFrame 内存是一个道理。

调用协议需要这样变化：当我们调用一个函数，我们不能够直接像 fib(n-1) 这样做。我们需要先**把函数调用返回后，要继续执行的内容，进栈保存**。
然后，再去执行 fib(n-1)。函数返回的协议也需要变化，函数完成后退出不是直接退出，而是需要去检查调用栈，出栈继续执行。

所以现在的执行上下文，需要增加调用栈。也就是(协程的)数据栈和调用栈，我们全部都自己管理了。

```
struct Cora {
	struct stack stk;
	struct cont *conts;
	int len;

	...
};
```

这里调用栈的单条记录我命名为 cont，其实叫 frame 或者啥的都无所谓。函数式语言里面对应叫 continuation 这样的概念。

cont 中需要存储哪些信息呢？需要有函数 fib 对吧，需要知道是 fib 内部 switch 中的哪一个 label，然后还需要知道使用的 fibFrame。比如说计算完 fib(n-1) 返回后，要计算的是 fib(n-2)，也就是下个 label 是 fib 中的 case 1，而等 fib(n-2) 也返回后，下一个 label 是 case 2。

所以我们记录的调用栈每一个 frame 的信息就是：

```
struct cont {
	void (*fn)(struct Cora *co, int label, void* frame); // 回到哪个函数
	int label; // 回到函数中的哪个 label
	void *frame; // 对应的栈帧使用哪个
};
```

现在的函数调用过程就是，保存调用返回后需要执行的内容，把当前的上下文信息设置成被调用函数，跳转到当前函数。

```
			// 保存 cont， 用于返回到计算 fib(n - 2)
			struct cont ret = {
				.fn = fib,
				.label = 1,
				.frame = frame,
			};
			co->conts[co->len++] = ret;
			
			// 保存好返回位置后就可以调用 fib(n-1) 了... 调用要提供新的 frame
			// alloc new frame for it
			struct fibFrame *xx = (struct fibFrame*)(co->stk.ptr + co->stk.len);
			co->stk.len += sizeof(*xx);
			xx->n = frame->n - 1;

			struct cont call = {
				.fn = fib,
				.label = 0,
				.frame = xx,
			};
			co->conts[co->len++] = call;
			return;
```

如果我们把函数调用返回后要执行的 continuation，和函数调用要调用的内容，依次进栈，那么执行流程就可以变成一直是从当前调用栈，弹出当前要执行的内容，执行它。这个可以写成一个 trampoline，顺手还把尾调用给支持了。

```
void trampoline(struct Cora *co) {
	while(co->len > 0) {
		struct cont* c = &co->conts[--co->len];
		c->fn(co, c->label, c->frame);
	}
}
```

完全的代码在这里

[v3.c](https://github.com/tiancaiamao/c-coroutine-demo/blob/main/v3.c)

```
void fib(struct Cora *co, int label, void *ptr) {
	struct fibFrame *frame = ptr;
	switch (label) {
	case 0:
		if (frame->n < 2) {
			co->res = frame->n;
			return;
		} else {
			// 保存 cont
			// 用于返回到计算 fib(n - 2)
			struct cont ret = {
				.fn = fib,
				.label = 1,
				.frame = frame,
			};
			co->conts[co->len++] = ret;
			
			// 计算 fib(n-1) ... 
			// alloc new frame for it
			struct fibFrame *xx = (struct fibFrame*)(co->stk.ptr + co->stk.len);
			co->stk.len += sizeof(*xx);
			xx->n = frame->n - 1;

			struct cont call = {
				.fn = fib,
				.label = 0,
				.frame = xx,
			};
			co->conts[co->len++] = call;
			return;
		}

	case 1:
		{
			co->stk.len -= sizeof(struct fibFrame);
			/* printf("return from result fib(n-1), n=%d, stk ptr = %d\n", frame->n, co->stk.len); */
			frame->val1 = co->res;

			// 返回到 +
			struct cont ret = {
				.fn = fib,
				.label = 2,
				.frame = frame,
			};
			co->conts[co->len++] = ret;

			// 计算 fib(n - 2)
			struct fibFrame *yy = (struct fibFrame*)(co->stk.ptr + co->stk.len);
			co->stk.len += sizeof(*yy);
			yy->n = frame->n - 2;

			struct cont call = {
				.fn = fib,
				.label = 0,
				.frame = yy,
			};
			co->conts[co->len++] = call;
			return;
		}

	case 2:
		co->stk.len -= sizeof(struct fibFrame);
		/* printf("return from result fib(n-2), n=%d, stk ptr = %d\n", frame->n, co->stk.len); */
		co->res = co->res + frame->val1;
		return;
	}
}
```

现在的调用栈和数据栈完全是受我们控制的了，我们有一个上下文来管理这些，这个上下文的结构体，我命名为 Cora，是因为 cora 语言本质上讲，就是这样的一个 C 语言协程实现的 runtime，只不过那边还额外多出来垃圾回收相关的内容。

我想强调的是，这是我发现的 C 语言有栈协程**最通用的实现方法**。它没有一行汇编，不依赖硬件环境，不依赖操作系统或者任何编译器的 feature。不止是 C，任何其它语言都可以用这种方式来实现有栈协程，只不过 C 语言是最契合的。


当然，也是有代价的。下面是一些性能测试数据。在 2023 macbook apple m3 下测试，使用 gcc(clang) -O2 编译。

|---------------------------|---------|---------|
|                           | fib(40) | fib(41) |
| v0 (初始实现)             | 0.29s   | 0.44s   |
| v1 (状态机写法)           | 0.65s   | 1.05s   |
| v2 (手动frame管理)        | 0.87s   | 1.39s   |
| v3 (手动调用栈+frame管理) | 2.76s   | 3.52s   |


由于我们完全是自己去管理调用栈，调用协议不再是使用直接使用的 C，而是用 C 语言来实现我们的协程，这里带来了一些性能开销。
主要是这几方面带来的：

1. 原始的函数写法被改状态机写法，状态中的每一小步都是可以中断切换上下文的，但是引入更多的状态和 switch case 会少量开销
2. 函数栈 frame 的管理，每次函数调用前准备 frame 和函数调用结束后的清理 frame 也带来一些额外开销
3. 最大头的开销是 trampoline 的引入
3.1. 调用协议不再是直接调用和直接返回，而是每次要保存 continuation，调用栈的 push 和 pop 开销
3.2. 函数调用多了好次几跳转过程，因为需要从当前函数退出到 trampoline，然后再次进入 trampoline
3.3. C 语言的函数传参由寄存器变成了内存传参，需要经过 cora 或者 frame 来进行参数传递，而不能直接使用 C 的函数参数

我个人认为还是可以接受的，毕竟为了支持这么通用的协程，没有函数染色问题，总得付出点什么。
相比之下，elisp 这种字节码解释器，执行 fib(40)，耗时 8.835s；python 耗时 19.8s；janet 耗时 16.13s，也就是说字节码解释器这种形式，是完全没法碰磁这种 transpile to C 的性能的。然后 guile scheme，带 JIT 编译的，提供了 continuation 机制的，执行 fib(40)，耗时 2.645s。速度算是在同一量级，但是在实现成本上可就是天壤之别了。
