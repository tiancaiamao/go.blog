刚开了一个新的 [repo](https://github.com/tiancaiamao/tidb-code-RAG)，搞了一个 tidb 的 RAG。

这个背景是这样的，当我们处理很大的项目的时候，coding agent 能否更好的理解项目，理解上下文和一些 feature 之间的关联，
以及可能有一些隐式的约束存在。人类写代码的时候，或者说 TiDB 资深的工程师写代码的时候，脑中大概是有一些更 high level 的知识的。而 agent 写代码的时候，
尤其是项目规律很大很复杂之后，容易只顾局部。所以想法就是，能不能 RAG 一下，给 coding agent 能提供更好的背景知识。

首先想到的就是卡帕西那个 auto wiki 类似的东西。我可以给 tidb 的代码，建立一个 wiki 起来。随着代码的变更，可以让 agent 维护这个 wiki 的变化。agent 作为 wiki 的编译器。

说到代码仓库的 wiki，网上类似的东西就有 [deepwiki](https://deepwiki.com/pingcap/tidb) 和 google 最近出的 [codewiki](https://codewiki.google/github.com/pingcap/tidb)。
只不过那边是在线服务，而我弄的是本地版本的一个 wiki。

在线版的 wiki 服务也还挺好用的，有时候我要去理解一个项目，直接去看 wiki，然后提问，就可以快速地掌握整个项目的脉络，比上古时代啃代码的方式要高效得多。
有时候我 clone 一个其它项目下来，让 agent 去分析代码的时候，会让 agent 先从这类的 wiki 服务抓取一个 overview，这比 agent 直接探索整个项目会节省 token 一些。
就相当于 codex 或者 claude code 中执行 /init 的过程。

那它跟直接在项目中写个 AGENTS.md 有啥差别呢？如果项目中创建过样的文件，然后也记录了仓库的整体的索引信息，那 coding agent 是不是也会得到同样的上下文？

还是有一些差异的。首先是会更省 token。如果 coding agent 加载 AGENTS.md 之后，继续把需要的内容一点一点加载进来，
这个来来回回的过程会更耗 token 一些。而我这边设计成一个冻结的 session，以 snapshot 的形态来提供服务。
也就是有一个子 agent 是直接从 session 中恢复出来，形成提供 RAG 服务的 agent。这个过程就不涉及多次加载文档形成 snapshot 的过程，
启动即包含全部的内容。

相当于是三层：source code -> wiki -> context。先是源代码，这是在外存的。然后由代码生成出来的 wiki，这些文件就放在这个 repo 里面了。
使用的时候，直接从加载好全部 wiki 文件的 session 中恢复出来，就是一个直接可用的上下文。

然后还有一个差异是 coding agent 和 RAG agent 隔离。如果我们要加载很多很多的 wiki 的内容，以便 coding agent 掌握更多上下文，
这确实也可以，但是信息多不代表最有效。我们需要在有限的 coding agent 的 context 中放置最有价值的信息。
如果 AGENTS.md 那边要加载特别多的内容，还不如让一个专门做 RAG 的 agent，来告诉 coding agent 它需要的内容。
由于 RAG agent 和 coding agent 分离了，在 RAG agent 的上下文中，可以塞尽量多的跟 wiki 相关的内容，
而 coding agent 那边，则只向 RAG agent 去查询它所需要的内容。

怎么构建？我分两条线让 agent 去 explore 整个 tidb 的代码仓库。主线是围绕整体的 archtecture 和各个子模块。
然后辅线是各个 feature 相关的内容。除了代码仓库，我还让 agent 把文档相关的也全部读了一遍，然后更新上一轮生成的 wiki。
这样这些 wiki 文件内容就构建好了。

然后就是 system prompt 以及用于加载的提示词，把构建好的 wiki 文件变成一个 session 的 snapshot。这样下次就不用重头构造，而是直接恢复 snapshot。

怎么维护呢？预期我会隔一阵子，来看一下它跟上次的 wiki 对应的 tidb commit 发生了哪些变化，然后根据变化的代码来更新 wiki 的内容。
一般来说，底层代码变化快，而架构和 feature 这些更高层的内容则更新不那么频繁，所以这里应该不会涉及信息很快过时的问题。
还有一个想法是，这个 repo 能把人类的知识也整合进去，希望会有一些手动的更新，把我自己给蒸溜了。

怎么使用？大概有这几种吧，一个是我自己把它当本地 wiki 一样用，就像问 deepwiki 那种，去提问来了解一些东西。
这比我直接读代码找答案会方便。

还有就是准备一个技能，这个技能就可以告诉 coding agent 怎么使用这个 tidb code RAG。它可以向 RAG 的 agent 提问，来了解它想知道的内容。
又或者，coding agent 要做一个什么任务，它可以让 RAG agent 给它提供这个任务所需要的上下文信息一次提供好，也是 RAG 的效果。

当前只是针对 TiDB 做的，但是方法和思路其实并不限于 TiDB 的 repo。
任何其它的 repo 都可以帮到类似的方法。不过那就是更通用的 deepwiki 了。
