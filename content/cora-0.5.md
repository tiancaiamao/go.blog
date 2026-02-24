距离[上次发版](/cora-0.4.md)已经过去了大半年。这段时间的改动，主线其实很清晰：为了多核并发，先把 runtime 和 GC 的地基重铺，再把并发模型真正落地到库和测试里。现在这些工作基本补齐，可以准备打 0.5 版本 tag 了。

这次改动可以概括成三块：

- runtime 重构
- 每个 Cora 实例独立 GC
- 多核并发支持

## 重大改动

### 1) runtime 重构

这一块对应之前那篇博客：[C 语言有栈协程最通用的实现方法](/c-stackful-coroutine.md)。

0.4 之前的生成代码是“大一统 dispatch”：把所有 basic block 塞进一个巨大的 switch 里，再靠切分和 computed goto 去优化。这个方式能跑，但有个本质问题：分支太多，控制流过于复杂，C 编译器很难做出理想的寄存器分配和局部优化。

0.5 的重构把模型改回“每个 cora 函数对应一个 C 函数”。函数调用协议变成：

1. 保存 continuation（callstack/frame）
2. return 让调度器接管
3. 通过 label 重入继续执行

这样做有几个直接收益：

- 生成代码结构更接近原始函数边界，可读性和可调试性好很多。
- 编译器优化空间更自然，`-O2` 下实测收益明显。
- 后续做并发调度、GC safepoint、跨 VM 隔离时，状态边界更清晰。

当然这次重构并不是一个 commit 就结束，而是一整串配套工作：代码生成更新、旧 runtime 清理、saveCont 抽取、continuation 泄漏修复等，基本把执行时基建翻新了一遍。

### 2) 每个 Cora 独立 GC（并发前置）

如果说 runtime 重构是“把车身换掉”，那 GC 这部分是“把底盘和传动一起换掉”。

旧模型里，GC 和 pthread 绑定得比较紧，还依赖 C 栈作为 root 来源。这个设计在单线程时代问题不大，但一旦做多核并发，马上会遇到几个硬约束：

- GC 边界和线程边界耦合太深
- Host 语言（C）栈参与 root 扫描，状态不够可控
- 多 VM 并行时，隔离性和可组合性都很难保证

0.5 里核心改动是：**每个 Cora 实例拥有自己的 GC/内存管理**，并在 trampoline safepoint 上做 root 管理，不再依赖 C 栈去兜底。这一点非常关键，因为它把 Cora 从“绑在线程上的运行时”推进到“可嵌入、可并行调度的 VM”。

配套还有两项很重要的重构：

- 符号绑定重做：去掉 thread local 依赖，改为符号与 VM 内部环境解耦映射。
- 生成代码多实例安全：同一个 `.so` 可以被多个 Cora 实例安全共享，不再因为静态全局状态互相污染。

这几块合起来，才让后面的多核支持不是“功能演示”，而是“可维护的架构”。

### 3) 多核并发支持落地

多核这块之前在博客里已经写过设计：[思考 cora 的多核支持(后续)](/cora-multicore-support-2.md)。0.5 的工作重点是把设计变成代码，并且把测试补上。

核心架构就是：

```
- M threads driving N VMs (VM-level parallelism)
- Multiple coroutines per VM (VM-level concurrency)
- CML for synchronization (perform/abort)
- Mailbox for cross-VM communication
- Channel for intra-VM communication
- Single global Poller for I/O events
```

这部分也是 cora 里首个 AI 深度参与的大 PR（[#131](https://github.com/tiancaiamao/cora/pull/131)）：大体结构和大量样板实现由 AI 生成，我主要负责设计约束、架构校准和收敛修正。
结论也很直接：AI 在“把明确设计铺成代码”上已经很好用了，但在 runtime/并发这类强约束系统里，还离不开人类做一致性把关。

## 中小改动

除了三块主线，这个版本还有不少“看起来小、实际很关键”的改动：

- 值表示从 tagged pointer 切到 NaN pointer（#126）。
- 宏展开最后增加 source-to-source 的 partial evaluation pass（#108），方向上类似 Chez 的 cp0，但实现更轻量。
- REPL 从 AST interpreting 调整为 closure interpreting（#130），明显提速，同时没有引入太重的维护负担。
- `import` 增加 `.so` 缓存策略（#114）：位于 `$CORAPATH` 的包会把生成产物缓存在同目录，下次优先复用。

这些点单看都不“炸裂”，但叠加起来，还是有一些改动量的。

## 引入 CI 以及测试加强

0.5 版本另一个明显变化是测试体系更像“工程项目”了，而不是“本地能跑就行”：

- 构建系统迁移到 CMake（#132），配合原有 Makefile，方便不同环境接入。
- GitHub Actions 接入 CI，push 自动跑 `make test`（#112）。
- 增加 bootstrap test（#115），验证 `init.c` 和 `lib/toc.c` 生成稳定性，专门防回归。
- 补充了 man-or-boy 等等一些其它 test，补齐函数式语言常见语义压力测试。
- 并发相关新增大量测试样例（mailbox、cross-vm、poller、parallel net 等），从 API 到端到端都覆盖了一些关键路径。

这部分虽然没有直接增加语言 feature，但它决定了后面还能不能继续快速迭代。尤其是并发和 GC 改动频繁的时候，没有回归测试几乎不可能稳定推进。

## 新引入的包和能力

0.5 期间，新包和库能力也在持续补全：

- `define-record` 宏：更方便地定义构造器、谓词和访问器。
- `lib/parallel` 及其子模块：mailbox、VM 管理、poller、并发网络基础设施，这个不用多解释，整个 0.5 都围绕它展开的
- `lib/net/http` 正式引入 HTTP 库（#134），为“把博客剩下的一半也能迁到 cora”补上关键拼图。
- `lib/handle-map` 用于 cora 和 C 交互的场景

http 库的引入是为了检验底下的这些并发基础设施是否可用了。也是一个"勿在浮沙筑高台"的事情，我可以自豪地说 0.5 版本，可能 http 本身是不太完善的，但是它下面的基础依赖却是相对足够健壮了。

另外像 handle-map 这类配套抽象，也是在并发实现过程中逐步沉淀下来的。它们不一定是最显眼的 feature，但对于 cora 跟 C 交互这块，还挺重要的。

- C 层如果直接持有 cora 对象指针
- 而 cora 侧没保留到该对象的 root 可达路径
- 那 GC 就可能把它回收
- C 层留下 dangling pointer

handle-map 就是把 cora 对象变成 handle -> obj 这样的关系，在 cora 层用数据结构保留，然后在 c 那一层只持有 handle 而不是 obj。
这样通过 handle 就可以安全地间接使用 obj 了。

## 拥抱 AI

cora 0.5 首次包含 AI 生成的代码。曾经我的想法是，我要创造一门语言，然后用我自己创造的语言写代码。写各种东西。
但是如今，AI 的发展已经快完全替代了人类，我也不再自己手敲代码了。最好的编程语言就是自然语言。那 cora 这个项目还有意义么？

还是有的，我的答案是，我们要拥抱 AI，用新的思维去解决问题。

比如说，[教 AI 学习 cora 语言](/teach-ai-to-learn-cora.md)，就是一个挺有意思的实践。因为这种小众语言，AI 是没有在训练阶段去接触的。AI 可以写 c，c++, python 等等。
但是我只要教会 AI，它就可以写 cora 了。

以前我们手敲一个一个库，很慢。但是有 AI 的加持之后，我们可以很容易地把其它语言的库 port 到 cora 语言来，这对于 cora 快速迈向 1.0 将会是一个加速度！
通过 AI，可以更快地创造出一门语言来。而基础库的完善，又使得这门语言可以用来做一些实际的开发和应用，相辅相成。
