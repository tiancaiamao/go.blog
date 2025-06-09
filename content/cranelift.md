关注 rust 的时候注意到 [cranelift](https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/docs/index.md) 这个 JIT 的库。它性能达不到 LLVM 的水平，但是代码生成的速度比 LLVM 要快得多(几乎世界上绝大多数的其它 JIT 都是这个卖点，比如 [qbe](https://c9x.me/compile/))。

真正让我稍微感兴趣并尝试一下的原因是它的 [IR](https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/docs/ir.md) 设计，不是像 LLVM 的 SSA 形式使用 phi，而是 block 之间的跳转，并且是带参数的跳转。咋一看这非常像适合 CPS 的 IR。

市面上其它所有的 IR 几乎都是按 SSA 形式设计的。其原因是在传统的编译器领域 SSA 已被证明是事实标准了，基于像 Go 这种不是基于 LLVM 的而是自己实现编译的语言，到后期它都重构成 SSA 形式的 IR 了。 CPS 和 SSA 有很强的联系，一般是函数式语言那边会采用 CPS 形式的 IR。由于整体函数式语言的小众，这些基于 CPS 形式的编译优化都是实现在某一门语言内部的，没有什么通用 JIT 是为函数式语言做这种设计，所以一般看到的都是 SSA 形式 IR。

[cora 为了实现 delimited continuation 就使用了 tailify 的技巧](tailfication-delimited-continuation.md)，实际上就是比较轻量的 CPS 变换，通过变换之后，最终的 IR 就是标签之间带参数的跳转。比如 fib 函数:

```
(defun fib (n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
```

最终生成的 IR 是:

```
((3 (lambda () ()
      (let x140308317999719 ((%builtin set) (%const fib) (%closure 2 1))
	   (return x140308317999719))))
 (2 (lambda (n) ()
      (let x140308318035623 ((%builtin <) n (%const 2))
	   (if x140308318035623
	       (return n)
	     (let x140308317999207 ((%builtin -) n (%const 1))
		  (call ((%global fib) x140308317999207)
			(%continuation 1 n)))))))
 (1 (lambda (x140308317999271) (n)
      (let x140308317999431 ((%builtin -) n (%const 2))
	   (call ((%global fib) x140308317999431)
		 (%continuation 0 x140308317999271)))))
 (0 (lambda (x140308317999655) (x140308317999271)
      (let x140308317999687 ((%builtin +) x140308317999271 x140308317999655)
	   (return x140308317999687)))))
```

尾递归会 tailcall 某个函数，而非尾递归就是上面这种 `(call ((%global fib) x140308317999207) (%continuation 1 n))` 的形式，意思是说，先调用函数 fib，调用前会需要把当前连续 continuation 保存起来，这里的这个 continuation 对应的是 label 1。函数调用返回怎么处理呢？它不会返回，而是恢复 continuation 继续执行。

经过 CPS 变换你会发现，所有的非尾递归的函数调用都消失了，都变成了尾调用形式，或者更准确地说，不叫尾调用，而叫带参数的跳转。
我们再看一看 cranelift 的 IR 设计，这里这个是 github 官方 repo 拿的一个例子：

```
function %average(i64, i64) -> f32 system_v {
    ss0 = explicit_slot 8         ; Stack slot for `sum`.

block1(v0: i64, v1: i64):
    v2 = f64const 0x0.0
    stack_store v2, ss0
    brif v1, block2, block5                  ; Handle count == 0.

block2:
    v3 = iconst.i64 0
    jump block3(v3)

block3(v4: i64):
    v5 = imul_imm v4, 4
    v6 = iadd v0, v5
    v7 = load.f32 v6              ; array[i]
    v8 = fpromote.f64 v7
    v9 = stack_load.f64 ss0
    v10 = fadd v8, v9
    stack_store v10, ss0
    v11 = iadd_imm v4, 1
    v12 = icmp ult v11, v1
    brif v12, block3(v11), block4 ; Loop backedge.

block4:
    v13 = stack_load.f64 ss0
    v14 = fcvt_from_uint.f64 v1
    v15 = fdiv v13, v14
    v16 = fdemote.f32 v15
    return v16

block5:
    v100 = f32const +NaN
    return v100
}
```

这个 IR 正是 block 之间，带参数的跳转！所以我想到的就是，能不能用 cranelift 来实现 cora 的 AOT。JIT/AOT 都无所谓，当前 cora 是将自己的内部 IR 再转成 C 代码，然后 C 代码编译成 so 了 load，这种形式的 AOT。如果可行，就有可能将 cora 改成使用 cranelift。所以就有了这次的体验和尝试。

遇到的第一个问题，block 其实是不能够取地址并作为跳转的 destination 的，block 作用域不能够跨函数。而我那边需求的带标签的 jump，是需要标签能够跨函数，比如说 `(call ((%global fib) x140308317999207) (%continuation 1 n))` 其实是想要调用函数 fib 之后，再跳转到另一个 label 1，既然 cranelift 的 block 实际不能取地址并跨函数使用，所以实际上我并不能够把 cora 当前的 IR 的带标签的跳转，转成 cranelift 的 block 间跳转。最终每个标签还是得对应到 cranelift 里面的函数，只有函数才是可以取地址并作为跳转目标的。

好吧，继续往下尝试，好在 cranelift 里面提供了 `return_call` 和 `return_call_indirect` 这样的指令，它是可以支持尾调用优化的，如果把每个标签都做成 IR 中的函数，再通过 `return_call_indirect` 来尾调用，这种做法似乎也还尚可接受。

然后验证，要做这个改动，其目标是什么？性能。所以我想再试试最基础的函数，看看能有多少性能提升。这个验证不需要把完整的 cora IR 都实现，只需要用 cranelift IR 实现一个简单的函数，以及在 cora 里面实现这个函数，两者相比较，就可以得到答案。继续拿 fib 这个函数验证。我之前有测试过 (fib 40) [在各语言下的表现](https://github.com/tiancaiamao/cora/issues/73)：

- 解释器递队: python 19.8s/ janet 16.79s/ elisp 8.835s
- JIT递队: guile scheme 2.645s
- 原生性能: go 534.322875ms

之前测试的时候 cora 需要 16.79s，不带gcc 的 O2 O3 这种优化级别编译。后来优化过一点点，但是又 regression 回去了，现在再测试 cora 需要正好 10 秒。验证过生成到 C 再编译成 so 这种模式，再开到gcc的 O3，极限大概 2-3s。所以如果手动写 [cranelift IR 验证一下 fib 40](https://gist.github.com/tiancaiamao/d62a0867f5293f1cf15db5696e7cea23)，速度是多少呢？答案: 614.51ms

好吧，这个速度倒是给了我一点点鼓舞，性能确实是原生那个梯队的。接下来想到另一个问题，其实我的实现是 VM 级别的 JIT，中间有一块我损失性能的地方。为了支持 delimited continuation，同时还为了非常舒适地调用 C 语言，其实我是引入了 VM 的。

cora 生成到 C 的代码，它实际完全不会使用到 C 的栈，而是自己管理自己的调用栈。它有自己的 valua 栈和 continuation 栈，value 栈是表达式计算会用到，而 continuation 栈则是管理函数调用之间的上下文，并且实现 delimited continuation。我不想把当前的实现叫 transplier，从来不说 cora transplier to C 而是说 cora compile to C，原因就是这里，cora 实际上是把 C 当一门通用汇编语言在用的，语言即虚拟机。它并不会一一映射成 C 的功能，所以不算是 transplier。

这样做的优势是调用 C 很方便，充分利用 C 语言本身的生态，嵌入到 C，就像 lua 语言那样。但是如果做 JIT 就能够感受到在 VM 这一层的 JIT 开销：因为堆栈的维护并不是使用原生 C 语言堆栈，函数的调用也不是走正常的系统 ABI 协议，调用前要保存 continuation 栈，这时就涉及边界检查，以及动态数组 growth。而且由于我对 value 和 continuation 分别使用了两个栈，边界检查和数组动态 growth 的事情还得做两遍。每一次的带参数跳转到另一个标签，都会涉及到这样的调用协议，所以是非常频繁的操作。这里对性能的影响会非常大，以至于即使使用 JIT 做优化，也会比原生代码慢许多。

想到这里我就放弃了，cranelift 并不能给我带来真正的收益，它的实现更复杂，维护代价更高。我可以肯定几十年之后，C 语言一定还在的。而在我有生之年，我不敢完全地肯定 cranelift 这个项目会一直活下去。最最不能接受的是，基于 VM 去做 JIT，即使使用了 cranelift 去优化，最终的效果也理论上达不到真正的最优，即达不到那种原生性能的级别。所以... 那我还不如这么干：

```
static void
fib(struct Cora *co) {
	int n = fixnum(co->args[1]);
	if (n < 2) {
		coraReturn(co, makeNumber(n));
		return;
	}
	int prev = 0;
	int sum = 1;
	for (int i=3; i<=n; i++) {
		int tmp = sum;
		sum = sum + prev;
		prev = tmp;
	}
	coraReturn(co, makeNumber(sum));
}

struct registerModule fibModule = {
  NULL,
  {
   {"fib", fib, 1},
   {NULL, NULL, 0}
  }
};

void
entry(struct Cora *co) {
  Obj pkg = co->args[2];
  registerAPI(co, &fibModule, stringStr(pkg));
  coraReturn(co, intern("fib"));
}
```

编译成 so，再 load，性能跟 C 是完全一样的，哈哈
