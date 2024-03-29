

栈虚拟机比较好实现，但是性能不如寄存器虚拟机。然而栈虚拟机有一个非常重要的好处是，对于 GC 实现更加友好。寄存器虚拟机要做 GC 的时候，在处理 GC root 时，很容易没 tracing 到某个变量/寄存器而被坑到。

举个简单的例子:

```
var x = callA(vm);
gc(vm);
var y = callB(vm, x);
```
这一段简单的代码里面，`x` 没被算到 GC root 里面，那 `x` 在 gc() 后就被回收了，到 callB 的时候可能就空指针啥的。

正确的写法需要这样：

```
var x = callA(vm);
vm.preserve(x);   // 需要额外保护临时变量不被 GC
gc(vm);
var y = callB(vm, x);
```

而如果基于栈做，不返回 x，而是 callA 的结果直接放到 VM 的栈上，就没这类问题了，因为临时结果放在栈上，而 VM 的栈肯定是会被当作 GC root 追踪到的，所以临时结果就得到了保护。Lua 的 API 设计在这个点上面就很巧妙。


lua 是一个基于寄存器的虚拟机，但是它的所谓"基于寄存器"等于是用栈模拟的的。寄存器有 255 个实际上就是 `R[i]`(栈或者说数组) 默认留 255 个糟位。
跟普通的栈虚拟机的区别是，普通的栈虚拟机大量用到 Push Pop 这类指令。而 lua 里面是在编译器把 offset 算出来了，直接就 `R[i] = x` 这种形式去使用，而不需要 `push(R, x)`。

我想在虚拟机或者说 IR 这一层还是采用栈虚拟机的架构，不过要做 code generate 到 C 语言。

如果只是生成这种代码，性能是比较挫的：

```
opLocalRef(vm, 0);
opPush(vm);
opPreCall(vm);
opGlobalRef(vm, makeSymbol("fact"));
opPush(vm);
opLocalRef(vm, 0);
opPush(vm);
opConst(vm, makeNumber(1));
opPush(vm);
opPrimitive(vm, 2, symbolGet(makeSymbol("-")));
opPush(vm);
opCall(vm, 2, fn_label_1);
```

哪怕是优化到 lua 的"伪"寄存器虚拟机的样子，生成出 C 代码也还是不够优雅，因为经过 C 语言编译之后， `R[x]` 还是不能直接能映射到物理寄存器。

```
R[1] = R[0]
R[2] = makeSymbol("fact")
R[3] = R[0]
R[4] = 1
R[3] = primSub(R[3], R[4])
```

编译到 C 之后，再编译成 native，对寄存器的使用效果比较差。所以我就在想，怎么样让栈虚拟机能有效地利用到寄存器。

(题外话，gambit scheme 那边它的虚拟机设计是有寄存器概念的，并不是一个栈虚拟机，所以生成到 C 之后性能还可以很牛X)

(再题外话一个，qemu 就是把 x86 的解析后，动态翻译成 C，由于目前寄存器分配算法都很牛X了，让 C 到新架构的 native 也没损失多少性能)

当然，**前提条件都建立到生成出来的 C 代码能否有效利用寄存器分配**！

于是我研究了一下这个问题，还真找到了很有价值的信息。R大在[知乎上面有一个回答](https://www.zhihu.com/question/29355187/answer/51935409)，提到了 top-of-stack-caching，它是可以让栈虚拟机使用到寄存器的。

栈顶缓存很简单，就是栈顶的元素实际用寄存器去存，而不用放到栈上。

最终的 vm 设计我打算留 4 个栈顶寄存器，值寄存器 val 也保留了，然后 pc 寄存器，栈需要个 sp 和 bp。手动转一下这段代码：

```
static void fn_label_0(struct VM *vm) {
  opLocalRef(vm, 0);
  opPush(vm);
  opConst(vm, makeNumber(0));
  opPush(vm);
  opPrimitive(vm, 2, symbolGet(makeSymbol("=")));
  if (vm->val == True) {
    opConst(vm, makeNumber(1));
    opExit(vm);
    return;
  } else if (vm->val == False) {
    opLocalRef(vm, 0);
    opPush(vm);
    opPreCall(vm);
    opGlobalRef(vm, makeSymbol("fact"));
    opPush(vm);
    opLocalRef(vm, 0);
    opPush(vm);
    opConst(vm, makeNumber(1));
    opPush(vm);
    opPrimitive(vm, 2, symbolGet(makeSymbol("-")));
    opPush(vm);
    opCall(vm, 2, fn_label_1);
    return;
  } else {
    perror("if only accept true or false");
  }
}
```

预期应该是得到


```
static void fn_label_0(struct VM *vm) {
  // 编译器优化能力很强，局部变量会被映射到寄存器
  // val 值寄存器
  // bp 栈基址寄存器
  // sp 栈顶寄存器
  // r1-r4 栈顶缓存寄存器
  Obj *bp = vm->bp;
  Obj *sp = vm->sp;
  Obj val, r1, r2, r3, r4;
  
  val = bp[0];
  r1 = val;
  val = makeNumber(0);
  r2 = val;
  val = primEQ(r1, r2);
  if (val == True) {
	val = 1;
    return opExit(vm);
  } else if (val == False) {
    val = bp[0];
	r1 = val;

    opPreCall(vm);
	val = makeSymbol("fact");
	r2 = val;
	val = bp[0];
	r3 = val;
	val = makeNumber(1);
	r4 = val;
	val = primSub(r3, r4);
	r3 = val;
	*sp++ = r2;
	*sp++ = r3;
    opCall(vm, 2, fn_label_1);
    return;
  }
}
```

这个算法不太复杂，应该是 **始终让栈顶的若干个元素由寄存器来表示** 就行。

我们有 r1 r2 r3 r4，那么 push 一下，就是 

```
r1 = xx
```

当前状态变为寄存器使用一个。

再次 push，则

```
r2 = yyy
```

状态是栈顶两个元素由 r1 r2 存储 ... 再 push 也是类似，直到寄存器 r1 r2 r3 r4 都用完了，这里就要发生 spill 了。
spill 处理也很简单，让最老的那个寄存器进栈，把寄存器给腾出来。比如这里就是

```
push(r1)
r1 = zzz
```

当前的栈顶就浊 r2 r3 r4 r1 了。再次 push 则是让 r2 进栈后把寄存器腾出来：

```
push(r2)
r2 = ccc
```

怎么知道是由哪几个寄存器，分别映射到栈顶的哪几个位置呢？ 把 [r1 r2 r3 r4] 看成一个环，由栈深度状态，加上当前指向谁，就可以确定了。

比如说栈深度是 4，r4 指向栈顶，那当前栈和寄存器的映射就是 r4 r3 r2 r1

```
r1
r2
r3
r4 <-
```

比如说栈深度是 2，r4 指向栈顶，那当前的栈顶和寄存器的映射就是 r4 r3

比如栈深度是 3，r2 指向栈顶，那当前的顶顶和寄存器的映射就是 r2 r1 r4

```
r1
r2 <-
r3
r4
```

最终非常类似于 [gambit-scheme](https://gambitscheme.org/) 的设计了，都是定义一个通用虚拟机，但是将虚拟机编译成 c 代码，再优化性能。
接下来我将在 cora 里面把这个实现一下。
