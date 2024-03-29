按照之前的计划，原本应该是用生成到 C 的方式实现，用 [top of stack caching](top-of-stack-caching.md) 优化编译。中途做到一半，通过这篇[博客](https://robot9.me/webassembly-threaded-code/)了解到 [wasm3](https://github.com/wasm3/wasm3/blob/main/docs/Interpreter.md#m3-massey-meta-machine) 的实现方式，觉得思路不错，结果把 cora 又双叕重写了。

即使是生成到 C，通过 top of stack caching 优化变量分配，从栈上到寄存器上，也不是一个轻松的过程。尤其是在 spill 之后跟 gc 的交互，又得注意将寄存器重新 save 回栈。虽然可以挑函数调用前，通过调用协议约定让寄存器全部回栈上，然后 gc 挑这种时间点做。但是在每次调用协议上寄存器回栈这一步又是绕不过的开销了。总之，**在优化性能和实现复杂性方面取得一个很好的平衡，是很难的事情**。所以当我读到 `tail_call_reg` 的 threaded-code 方式之后，觉得这是一个更简单实现的性能与复杂度的平衡，因此便重写了，回到解释器方式实现了。理论上，generate to C 的性能的上限会比这种方式高，但是是通过更高的实现复杂度获取的，如果 interpreter 就能得到足够满意的性能，是值得去做的。
 
`tail_call_reg` 真是一个不错的东西，尤其是当函数调用协议本身是寄存器传参的时候，比如在 x86 下面的协议，前 6 个函数参数都是用寄存器传参的(RDI, RSI, RDX, RCX, R8, R9)，就相当于可以保存 6 个寄存器。不同的系统/体系架构下面，调用协议对寄存器使用的约定不一样，不过基本上都会有一些可用的寄存器。我把bytecode函数定义成这样子：


```
static void opCodeXXX(void* pc, Obj val, struct VM *vm, int pos);
```
这样最常访问到的指令寄存器pc/值寄存器val/栈顶寄存器pos，都是一直保持在寄存器中的。那些频次更低一些的，就通过 vm->xxx 去间接访问了。

目前里面实现的字节码就这么几个，其中以 opPrim 开头的是出于性能考虑把最常用的一些函数也实现成字节码了，它们其实可以用 API 方式去实现，只是调用协议上面性能开销会大些。

```
grep '^op' vm.c
opConst(void* pc, Obj val, struct VM *vm, int pos) {
opLocalRef(void* pc, Obj val, struct VM *vm, int pos) {
opLocalSet(void* pc, Obj val, struct VM *vm, int pos) {
opGlobalRef(void *pc, Obj val, struct VM *vm, int pos) {
opTailCall(void *pc, Obj val, struct VM *vm, int pos) {
opClosureRef(void* pc, Obj val, struct VM *vm, int pos) {
opExit(void* pc, Obj val, struct VM *vm, int pos) {
opPush(void* pc, Obj val, struct VM *vm, int pos) {
opIf(void* pc, Obj val, struct VM *vm, int pos) {
opCall(void* pc, Obj val, struct VM *vm, int pos) {
opMakeClosure(void *pc, Obj val, struct VM *vm, int pos) {
opReserveLocals(void* pc, Obj val, struct VM *vm, int pos) {

opPrimSet(void *pc, Obj val, struct VM *vm, int pos) {
opPrimSub(void *pc, Obj val, struct VM *vm, int pos) {
opPrimAdd(void *pc, Obj val, struct VM *vm, int pos) {
opPrimIsString(void *pc, Obj val, struct VM *vm, int pos) {
opPrimCar(void *pc, Obj val, struct VM *vm, int pos) {
opPrimCdr(void *pc, Obj val, struct VM *vm, int pos) {
opPrimCons(void *pc, Obj val, struct VM *vm, int pos) {
opPrimIsCons(void *pc, Obj val, struct VM *vm, int pos) {
opPrimGenSym(void *pc, Obj val, struct VM *vm, int pos) {
opPrimIsInteger(void *pc, Obj val, struct VM *vm, int pos) {
opPrimIsSymbol(void *pc, Obj val, struct VM *vm, int pos) {
opPrimNot(void *pc, Obj val, struct VM *vm, int pos) {
opPrimGT(void *pc, Obj val, struct VM *vm, int pos) {
opPrimLT(void *pc, Obj val, struct VM *vm, int pos) {
opPrimMul(void *pc, Obj val, struct VM *vm, int pos) {
opPrimEQ(void *pc, Obj val, struct VM *vm, int pos) {
```

[这篇博客](https://sillycross.github.io/2022/11/22/2022-11-22/)里面提到，tail-call 方法最大的问题是，callee-save 寄存器。由于bytecode函数仍然是函数，即使是使用尾调用的方式直接变成了 jmp，它仍然绕不过在函数退出时，需要恢复 callee-save 的寄存器。所以一旦 bytecode 函数需要用到 callee-save 寄存器，这块的开销就省不掉。它的解决方式是，不使用 C 语言的那套调用协议约定，而是用 C 来描述 bytecode，生成 IR 之后交给 LLVM，LLVM 中可以选择用 haskell 之类的其它的调用协议去生成代码，这类协议中没有 callee-save 寄存器的使用约定，于是就能绕开这块开销。数据结果还是很 promising 的，在不用手写汇编的前提下，达到了 LuaJIT 那种手写汇编的性能。我不需要能到 JIT 的性能，到正常 Lua 解释器的性能也就够用了。

重写除了改用 tail-call 实现成解释器，另外一块工作就是实现 bootstrap 了。纯 vm 实现的部分用的 C，[compiler 用 cora 语言来实现](https://github.com/tiancaiamao/cora/blob/3671f25ad06e72b66e354513c5ac71833f79b22f/lib/compile.cora)。

实现自举都会遇到先有鸡还是先有蛋的问题。我这边的做法是：

- 第一步，先用 Go 实现了一遍，这样就得到一个 Go 版本的初始的 cora，先把字节码跑通。
- 第二步再把 Go 实现的字节码部分 port 到 C 实现，确认这块能跑通。也就是用 Go 实现生成出来的字节码，在 C 实现的 vm 上面能执行。
- 第三步，用 cora 语言实现自己的 compiler，这样就可以使用 Go 的实现加载 compiler.cora 生成字节码，然后用 C 的 vm 加载代码做测试。
- 第四步，也是自举的最后一步，把 compiler.cora 编译出来，生成字节码，确认 vm 能正确执行这块字节码，整个 bootstrap 过程即完成。

后续的版本不再要求 Go 的参与，因为有了 C 实现的 vm，用 vm 加载初始的 compiler.cora 的字节码文件，即得到完整的编译环境。

实际的过程并没有这么顺利，调试花了挺久的。tail-call 的写法其实挺不"调试友好"的，因为尾递归完全丢失了栈信息，而栈信息在调试是其实非常有用。另外，这个写法还导致通用性丧失了，gcc 的 -O2 是确定可以把这种尾递归写法生成 jmp 的，但并不代表所有的 C 语言编译器都能实现尾递归的优化(LLVM没尝试不确定)。而且必须开到 O2 以上优化，否则编译器没将尾递归优化成 jmp 会导致递归调用爆栈。还有一个矛盾就是，开 O2 优化跟 -g 调试信息之前的冲突，优化之后很多代码被优化掉了，导致调试时没法单步跟代码。

感觉能完成 bootstrap 特别重要的一点就是我先实现了 Go 的版本，这样在 C 那边能参照。很多场景下 gdb 的 debug 能力靠不住，我都是靠找到最小的复现 case，然后在 Go 那边跑，再到 C 这边对照，从而去发现问题。
举个例子，我发现一段 cora 代码写得短一点的时候，它就能通过，而代码长到一定程度，就会挂掉。由于尾递归优化掉了栈信息，挂的时候就不知道从哪挂掉的。根据我丰富的调试经验，就猜想有可能调用栈的大小是引起挂掉的临界条件，于是去好好一检查，果然，当时固定分配了一定的大小，还在代码里面留了个 TODO 就坑自己了。虽然有些 bug 不好调，但总是能找到复现的 case，能够稳定复现的 bug 都是好 bug，比起 gc 相关的 bug 的调试，那还是很幸福的，后者经常遇到即不易复现，还存在 panic 的时候已经离 bug 场景很远了。

关于可维护性方面，bootstrap 方式应该是不如直接用 C 来实现 compiler 的方式的，修改了 compiler.cora 的实现不能够立刻反映到当前的 repl 环境，需要重新将 compiler.cora 生成字节码镜像文件，然后让 vm 加载新的镜像，开始新的 repl。
而如果这里面有 bug，加载过程就很难 debug 了。不过也有好的一方面，说是把 vm 做稳定，并且体积更小，整体更加模块化；然后 bootstrap 的过程也是迭代开发中的测试过程，下一次的改动要基于上一次的结果，能 bootstrap 起来说明不会把一些地方改坏了却没有任何察觉。

再接下来的开发，应该不会重写这样的大改了，先是把 try-catch 改回来，然后 gc 改回来，再就往上层堆功能了...
