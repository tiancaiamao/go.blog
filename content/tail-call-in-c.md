C 语言本身是不支持尾递归的，语言标准层面没这样的要求。像 scheme 之类的语言，严格的尾递归要求，是写进语言标准规范的。那么将 scheme 之类的语言编译到 C，就面临着如果支持尾递归的问题。我自己的语言 cora 目前也是编译到 C 的实现。从 gambit scheme 那里学到的一个技巧，记录一下。

首先，第一种方式是 gcc 开启 O2 的编译，编译器还是能够把部分场景优化成尾递归的。在我[上一个版本里面尝试过](cora-interpreter.md)。但是这种有一个弊端就是

> gcc 的 -O2 是确定可以把这种尾递归写法生成 jmp 的，但并不代表所有的 C 语言编译器都能实现尾递归的优化(LLVM没尝试不确定)。而且必须开到 O2 以上优化，否则编译器没将尾递归优化成 jmp 会导致递归调用爆栈。还有一个矛盾就是，开 O2 优化跟 -g 调试信息之前的冲突，优化之后很多代码被优化掉了，导致调试时没法单步跟代码

然后，第二种方式是把所有的函数代码写在一个大的函数里面，用 switch 或者 goto 的方式跳转。因为是在一个函数内，所以就没有尾递归的问题了。其实尾递归的调用协议就是"带参数的 goto"。这里面 threaded code 的方式示例代码:

```
void hugeBody() {

static labels = {&label1, &label2, &label3 ...};
int pc;

label1:
{ ... }

label2:
{ 
  ...
  pc = 3;
  goto labels[pc];
}

label3:
...
}
```

尽管跳转的效率很高，这种方式的问题是，由于所有代码全部放到一个函数了，编译器的编译过程非常耗时，而且也没法支持模块化编译。函数体过大之后，这种形式理论上对编译器的寄存器分配算法也造成很大麻烦，生成的代码不一定会比较优。

第三种方式，使用蹦床 (trampoline)。每个函数还是分开写成一个一个的，然后由 trampoline 去驱动：

```
void label1(struct Ctx *ctx) {
  ...
}

void label2(struct Ctx *ctx) {
  ...
  ctx->pc = label3;
  return;
}

void
trampoline(struct Ctx *ctx, basicBlock pc) {
  ctx->pc = pc;
  while(ctx->pc != NULL) {
    ctx->pc(ctx);
  }
}
```

trampoline 的优点是它是最为通用的办法。但是对比前一种情况可以发现，它的性能不是最优的。在函数退出前需要设置下一条函数需要执行什么，`ctx->pc` 这个是非寄存器的赋值。然后 `label2` 函数的退出会涉及 ret 指令，这是第一次跳转。
退出函数后，它是在 while 语句里面的，while 会有条件检查指令和跳转。接着调用 `ctx->pc(ctx)` 这又涉及一次 call 指令。如果从汇编这种很微观的角度观看，trampoline 的y方式实现尾递归的开销，是远高于一条 goto 指令的。


技巧就是：灵活地将第二种方式和第三种方式混用!

可以将一块代码块的地址，就成 pc + label。其中 pc 用来指向一个函数，使用 trampoline 方式。而在一个函数内部，则使用 goto 跳转，许多小块的函数写到一起的，由具体的 label 再去定位是哪一块代码段。

```
typedef basicBlock* (*fnptr)(struct Ctx *ctx, int entry);

struct basicBlock {
  fnptr pc;
  int label;
};
```

外层的 trampoline:

```
void trampoline() {
  basicBlock *pc = initial_pc;
  while(1) {
    pc = pc->func(ctx, pc->label);
  }
}
```

内层的，写个 switch case 的示例而不是 threaded code 版本的:

```
basicBlock* xxx(struct Ctx *ctx) {
  goto dispatch:
    switch (lbl) {
    case 0:
    case 1:
      ...
      pc = closureCode(xx);
      if (pc->func == ctx->pc) {
    	return pc;  // 大跳转走 trampoline
      } else {
	    goto dispatch;  // 小跳转还在 xxx 跳
      }
    case 2:
      ...
    }
}
```

分组可以用户自己决定将多少代码块打包一个函数。比如说可以直接 20 个或者 100 个拼到一个函数内的，这种是按批的方式。
又或者是编译到 C 语言之前的函数，被编译后变成了多个代码块，这些代码块打包拼凑一个函数内;也可以更多内容打包，比如一个模块全部的函数都只打包到一起。

只要是在内部跳转，代价是比较低的。而跨块的跳转走 trampoline 相对的代价就会高一些。

按照[论文 Compiling higher-order languages into fully tail-recursive portable C](https://www-labs.iro.umontreal.ca/~feeley/papers/FeeleyMillerRozasWilsonDIRO1078.pdf)中的说法，这样将 scheme 编译到 C 的性能大概就是 C 的 2-4 倍，这种级别的速度还是挺强的。跳转这种代价大概占到了程序整体代价的 8% 左右。
