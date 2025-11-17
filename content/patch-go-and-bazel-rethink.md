之前我写过一个[手动 goroutine 调度器](/go-manual-scheduler.md)，出发点是想解决产品里面 Go 的调度导致的长尾延迟问题。尤其是在大小查询混合/前后台任务并发之类的场景下面、低优先级的任务会挤占高优先级任务的资源，增加延迟。这个事情推进很困难，一度搁置着。其中一个很重要的原因是，实现强依赖于 patch Go。这个 [patch](https://github.com/golang/go/pull/51347) 是 cockroachdb 提的一个 PR，给 goroutine 增加了 track on-cpu 时间的功能。cockroachdb 那边 22 年的 2 月份就提出了这个 PR，但是官方迟迟没有合入主线，到 22 年的 11 月份时，他们的官方博客介绍了他们的 [Rubbing control theory on the Go scheduler](https://www.cockroachlabs.com/blog/rubbing-control-theory/) 时，这个 PR 依然没有合入，看起来没什么希望。Go 官方对于外部 PR，尤其是像这种非 bug 修复的 PR，并且也不是让每个用户能受益的 feature，感觉是不怎么热衷。我发现真正的用户里面，可能就我们和 cockroachdb 这种需求比较强烈，都是做数据库的公司。

如果 PR 不合，就只能想 patch Go 之类的歪路子了。所以就来聊一聊 patch Go 这个事情。

cockroach 维护了他们自己的 Go，patch 了更多的东西，重新[源代码打包](https://github.com/cockroachdb/cockroach/tree/master/build/teamcity/internal/release/build-and-publish-patched-go)，用自己维护的 Go 去编译出 cockroachdb。我们仅仅需要这一个 patch。该 patch 主要修改的是 runtime 包下面的[文件](https://github.com/golang/go/pull/51347/files#diff-94cc29f0d4af9ff5b6705956083683ed640829ddd9aec3d6ca10969eae9a71bb)，一开始我以为给 Go 打 patch 需要重新编译 Go，这样在维护成本上就很复杂。后来试验了一下发现，**如果是 patch runtime 而不是 patch 编译器，其实是可以不重新编译 Go 的！**

所以 patch Go 就很简单了，只需要为各个版本维护 patch 文件，然后用一个脚本去 patch 相应的 Go 代码。[脚本](https://github.com/pingcap/tidb/pull/44545/files#diff-5daa78b2314e27ba519ca081db326d22b69cc852c960117ff72936484acb4ab6) 也很简单，就是定位到 GOROOT 安装目录，确认 Go 的版本，然后用对应版本的 patch 文件，执行 `git apply patch`。用户的代码编译，链接 Go runtime 包的时候，会重新编译 Go runtime 包，使用 patch 后的代码。

很简单对不对？不对！真正的复杂度是在技术之外的。怎么样说服大伙同意 patch Go。因为修改 Go 影响的不是一个 repo，涉及到了对所有项目的影响，就跨了好多个部门。负责 CI/CD 相关的是 EE(Engineering Efficiency) 的团队，需要拉大家一起来开会。如果只有 tidb 一个 repo 需要去 patch Go，而其它的项目都不需要，那其它部门可能就不怎么热心。另外，如果这个改动导致需要维护官方的，和 patch 的 Go 两套环境，那么维护的代价又是一个被质疑的点。再者，需要对这个事情的收益达成一致。比如，我可以说，patch 是为了引入 goroutine 调度能力，引入调度能力是为了解决大小查询混合负载下，小查询受大查询的影响，延迟抖动问题，那么 PM 可以说，部署多个 tidb 节点，把大小查询路由到不同的节点上面，物理隔离，一样可以解混合负载抖动的问题，还解决得更彻底。尤其是 tidb 是计算层无状态节点，随用随起，这更符合 serverless 思想。所以想要把故事讲好，就必须说，他还能解决这个，还能解决那个，还有一堆其它的好处，这又需要去找一些支撑。

**对他人看不到直接的好处，又需要对方配合并付出一定的工作量的任务，是最难推得动的**。尤其是跨了多个部门，这种场景可能要通过 leader 的 leader 的 leader 去推，那就要说服好几层。
其实不同的视角，看到的问题并不一样，所以让大家都达成共识很难。就说维护代价这个点，我就深有感触，同时得对引入 bazel 进行反思。

最初我们没有用 bazel 这样的编译工具，而是直接使用 Go 的编译。随之测试越来越多，CI 越来越慢。也尝试过一些执行上面的优化，但是编译耗时是一个绕不开的点。在本地环境上面，执行一遍测试要到半小时以上了，光是编译出所有的测试 binary 就要花上 10 分钟。bazel 的分布式缓存可以解决这样的痛点，只要能命中缓存，就能避免大量的重复编译，提高速度。这就是最初这个事情的 sell point。Go 本身是不支持分布式缓存的，单机的缓存在 CI 的场景下就不够用，bazel 可以支持分布式缓存。

引入 bazel 当初也是一个很难推动的事情，我们有一个同学算是 bazel 方面的专家，在主力推这个，但他并不是 EE 团队的。这意味着跨了很多部门，并且要拉上 EE 开会。起初我也是表示支持，并率先为相关的 PR 点赞的。现在则会反思这件事情：bazel 引入的复杂度还是太高了。

让我们来辩证地看问题。k8s，cockroachdb，等等这些大型项目都是在用 bazel 了，既然有这么多其它公司站台，bazel 本身的成熟稳定性是经过验证的，tidb 也越来越大，切换编译工具似乎也合理。bazel 刚引入的时候，也达到了它的预期效果：可以命中编译缓存，编译的耗时大大缩短，可以算 10x 甚至 100x 的提升。但是慢慢地，问题越来越多地暴露出来。比如说 cache 越来越大，单机的文件系统已经放不下了，又需要一个分布式的文件系统来存放 cache，这里又引入了一些复杂度。中间可能出现网络波动或者一些环境问题，导致 CI 的失败率上升；bazel 的并发设置问题，会导致测试平台集群资源使用不合理，EE 的同学没有精通 bazel 的，学习成本又上升了，我们需要 EE 的同学去维护这套编译链，是不是代价大于了好处。现在问题回到了原点，CI 真的变好了么？没看到变好的迹象，只能出问题相互推委。

bazel 引入后，这次我要修改 patch Go，就得维护两套场景，一种是直接 Go 编译，一种是 bazel 编译。迫不得已就得去看一看 bazel 那边的脚本怎么改，就改这一个小小的东西，研究[文档](https://github.com/bazelbuild/rules_go/blob/master/go/toolchains.rst#go-host-sdk)就花了一两天。
首先我要弄明白，bazel 的 go 源代码管理，它是有一个 go sdk 的，如果用 `go_download_sdk`，就是指定 Go 从哪儿下载哪个版本的源代码，这种方式 patch Go 我们就要按 cockroach 那种模式维护自己每个版本的 Go 源码，每次上游 Go 更新就需要重新 patch，重新打包并上传，再修改 bazel 配置使用新的 patch。还可以使用另一种，`go_host_sdk`，这种是直接使用环境里面已经安装的 Go，这样就有机会把直接 Go 编译，和 bazel 编译归并成一套 Go 代码。

接下来我要确认，bazel 是能够重新编译 patch 过的 go runtime package，而不是重用 cache，这里面又有哪些参数需要调整。再遇到一个地方是 Go 的条件编译，发现用 bazel gazelle 生成出来的 BUILD.bazel 并不支持非 Go 默认的 build flag。这个是请教了我们 bazel 专家，才解决的，不然换成我一直找文档研究可能都没法查清楚为什么。

**也许对于本身精通 bazel 专家，整件事情半个小时事情就能搞定了**。但是对于我来说，就是深刻地感知到 bazel 的学习成本，并且感慨 **bazel 引入的复杂度还是太高了**。试想，如果组里唯一精通 bazel 的同学离职了，又怎么搞？接下来该如何维护？我们固然可以说，CI 的问题不是由 bazel 引入的问题，bazel 的引入是为了解决问题，只是解决这个问题的过程又引入了其它多个问题。究其原因，不是 bazel 工具的错，而是我们对 bazel 的掌握不够导致的错。

同理心推及到 patch Go 这个事情上面，我可以算 Go 方面的专家了，我知道这个 pr 改了啥，我怎么 Go runtime 怎么运作的，哪 pr Go 再升级到各个版本，我也能够把相应的 patch 生成出来。
但是如果不是 Go 的专家呢？他们怎么看待 patch 这件事情？他们如何去维护后续的升级，会不会出现版本被锁死到某个 patch 过的版本上面？(cockroach 自己维护的那个 Go 目前还在 1.19，而官方的 Go 已经到 1.20.x 了)。

如果以一个新手的心态，patch Go 这无疑一件非常复杂的事情。新手也会觉得复杂，**patch Go 引入 的复杂度太高了**。如果以一个新手的心态，我跟着 bazel 官方文档走下来，执行到这里就遇到了因为墙内导致的报错：

```
bazel run //:gazelle update  -go_prefix github.com/tiancaiamao/sched
2023/06/14 11:08:51 could not resolve the version 'latest' to an actual version number: unable to determine latest version: could not list Bazel versions in GCS bucket: could not list GCS objects at https://w
ww.googleapis.com/storage/v1/b/bazel/o?delimiter=/: could not fetch https://www.googleapis.com/storage/v1/b/bazel/o?delimiter=/: Get "https://www.googleapis.com/storage/v1/b/bazel/o?delimiter=/": dial tcp 172
.217.163.42:443: i/o timeout
```

但是翻墙不是最基础的技能么？对于新手来说并不能这么假设，一定要假定学习成本！
所以当我们做一个事情，难以推动时，是不是应该以新手的心态换位思考一下：收益是否有预期那么高，而维护成本是否是想象的那么低？
