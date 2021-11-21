在群里讨论的碎碎念，觉得可以记录一下，作为思考的沉淀，用互联网黑话好像叫做“你的底层逻辑是啥？”。

## 关于 OKR

事情的起源是我们制定团队的 OKR，上面想拍一个 85% 测试覆盖率的事情，让我们承接。

我感觉这个是需要 argue 的。弄混了**目的与手段**。其实关于 Objective 我们能很容易达成共识，不管是高层还是具体到我们团队内部，大家都想要的是提升软件质量。
但是对于 Key Result，关于覆盖率 85% 那种，不能那样玩的。衡量质量的结果，应该是 bug 变少了，oncall 少了，用户表示我们的产品更稳定这类的指标。覆盖率 85％ 这是我们没找好衡量指标，只好用考核形式来证明我们做了什么事情...

> “我高考很努力，但是我没考到高分”
> “你看衡量指标，我每天写作业写到晚上 11 点，周末还去补课"

所以其实是对于如何实现质量提升的事情，上面拍脑袋想了一个路径。毕竟其它的路径也难找，而考核起来也复杂，代码覆盖率好量化。
但是如果上层“懒政”的，下面不能把脏活累活自己扛了...用“战术上的勤奋掩盖战略上的懒惰”，感觉是需要一点向上管理吧。

至于向上管理，我们先想一些可以提升质量的具体做法，然后让上面参考定义衡量指标，我们再看衡量的可行性。

OKR 有个概念叫”对齐“，对齐这个事情就应该是，大家共同的愿景是把"产品质量"的事情做起来...上面想清楚结果是怎样衡量的，下面想到具体的实现路径...
然后一拍，发现这个事情可以这么做...这就是对齐了。对于结果和衡量和实现路径，中间需要一些来回的沟通。

这里是 OKR 和 KPI 的关键区别：不是上面摊一个任务(85%测试覆盖率)下来，而是大家一起为了共同的愿景去寻找解决方案。

然后我就去群里提议，大伙至少要集思广益下，找个可行替代方案。所有 team member 大伙赶紧想个点子，否则就是把任务分工下去了。
需要是”能够考高分的点子，而不是写作业和补课的点子“。我问一问大伙想不想扛 85% 测试覆盖率的事情...如果不想的话，期望的就是，“能找出更好的衡量指标，能找出怎么实现的路径”。

## 这个事情有多难

有些类型的软件，比如 windows 这种面向用户的，只要重启能解决的事情，那都不算个事儿，毕竟"蓝屏"的钙才是好喝的钙，大家重启下，吐糟几句，也就过去了。

有些类型的软件，比如属于提供服务的类型，掌控是在自己手上的，出现 bug 后，只要在最快时间内能修复 bug 再上线，就可以控制影响范围了。

但是做数据库的不行。数据库是面向企业的，企业服务的命根子，如果企业服务挂了，就会影响很严重，对于直接用户的影响会放大 "受影响用户数量" 那么多倍。想想支付宝或者 12306 这种挂了，会影响多少人。
作为一家做数据库的 ToB 的企业，质量是必须在发版本前保证的，不能像提供服务那种类型做事后弥补方式。一旦版本发出去，被客户使用了，影响的其实是 "使用数据库的企业数量 x 企业服务的用户的数量"，这个是很恐怖的事情。
更不用说如果数据库结果出错了，那更是灾难了。任何一点点很小的 bug 经过这样的放大之后，都不再是小 bug 了。

影响的严重程度很高，这是一个特点。对于已经发出去的版本，难以弥补，这也是一个典型特点。因为数据库产品交付之后，出的 bug，再要求客户升级修复，是一个很折腾的事情，互联网企业迭代快些，传统金融类的要求就更高。

bug 是不可消灭的。所有问题都是复杂性引起的，而软件的复杂性是无解的问题，是人写代码就会有 bug。

关于难度我可以做一个比喻，有些产品的质量要求，就像是小学数学；而有些产品的质量要求，就像是中学数学；而对于数据库这这，就像是高等数学。同样是满分 100 分的卷子，但是考试的难度是完全不在一个级别的。

## 指标的讨论

把问题抛到群里面之后的一些讨论。有同学提到”我觉得路径好找, 指标不行“。还有些比如

> "产品质量其实挺难衡量的，之前有用过通过故障分来衡量的系统，我们不是有bug jail么，这个作为衡量指标合适么？"

出 bug jail 这种不行... 这种是被动的...

解释一下 bug jail 机制，这个是我们的流程管理上面的产品质量手段。每个团队负责的一些模块，对于已知 bug 会根据 bug 的级别，critical major minor 这些分级，不同分级用不同的权值计算出一个分数，当这个分数过高，就要求团队停下来，不能开发新 feature，只能修复 bug 将分数值降下来。这个机制是没问题的，但是我们得有一些主动策略，改善被动局面。不是等到 DI 值升上去，再修 bug，而是想办法主动消除 bug。

> "其实质量这个东西是有点看天的，你可能啥都不做，一年什么故障都不发生"

什么都不做肯定不行。根据墨菲定律，只要有 bug，并且我们自己不把它先抓出来，那肯定是会被某个用户触发的。

> "用OKR来衡量的话，O是提高产品质量，KR就是降低cirtical bug的数量，降低新版本on call bug的数量，降低新版本bug fix的数量之类的。然后具体怎么做的话，就可以是写测试代码，提升测试场景之类的，提升测试覆盖率之类的。"

> "那比如降低 critical bug 的数量、降低 5.2 版本 on call bug 的数量之类的？"

如果只是单纯地减少现有 oncall issue 数量，降低 DI，这个是"被动"的，做得远远不够。

不能靠投入更多人力在临时解决客户问题上面，这是被动挨打局面。资深研发都去支持客户了，实习生是写代码的主力。大家疯狂堆 feature，质量把控不到位，然后客户遇到问题更多，恶性循环。
也不以靠出了 bug 修 bug，维持 bug 数量不至于太夸张这种佛系做法。

> "我个人看法是万金油指标基本上找不到, DI/oncall/issue之类的已经是极限. 人都说不准的事情, 量化怎么可能有用, 不免变成类似应试教育的情况"

> "质量到最后还得靠人去搂, 一个项目一种做法, 不然说起来太空了. 具体谈类似的feat才能总结类似的衡量方法。所以我个人更偏好直接去简化架构, 那量化指标没用的时候, 我做质量还更简单可控。"

关于指标的讨论也没有达成什么一致的结论来，倒是引发了我对于工程流派的思考。

## 工程质量的流派

我发现不同的人，对于提升产品质量这个目标，对实现路径相差很大。这里有一个很好玩的发现。
从大类流派的区分可以有：软件工程流派；测试流派；代码品味流派...

软件工程流派，这个流派希望从整个软件开发的流程上面去把控，通过软件开发的方法论，来达到软件质量的目标。瀑布模型，敏捷等等。他们认为任务可以一层一层的拆解，只要在流程上面到位了，就可以保证结果。而软件工程流派对于”个体“是不信任的。
人都是可替换的，关键的是方法和流程，换一只阿猫阿狗，或者换一只猴子过来写代码，也一样能开发出产品。所以这个流派会特别多的条条框框。设计要有设计阶段文档，然后研发阶段会进度把控，风险管理，然后测试阶段，再有发版本，再维护阶段和产品复盘。

这个流派的人，跟管理路线的人，高度重合。相比于代码或者技术细节，更加倾向宏观和高层的东西。有时候会被下面认为比较务虚。尤其是因为不需要亲自写代码，有些"拍脑袋"想法的时候。

测试流派，这一派更“务实”一些，应该是真正的中坚力量。当然，如果只是一味追求测试覆盖率的那种，应该是还没有太上道的。

测试流派里面，又有不同的小类别细分。

有一类别是工具流派。重心是自动化测试，通过工具去发现更多的 bug。
如果入力投入在测试工具，并且产出是这个工具找到了我们多少 bug...这个事情是可以证明在提升质量方面投入的工作的。
而且有些组合的情况太多，很难覆盖到所有场景的，确实需要一些 randgen 的方法去覆盖更多的 corner case。所以 improve 测试工具可以是其中一种路径。

> "自动生成测试我觉得确实可以提升一下。前几天和明哥聊的时候也谈到了质量问题，比如现在temporary table的GA，我们虽然做了 GA matrix 的测试，但是很多 corner case 我们都没有信心覆盖到。但是我觉得关键是如何提升测试的规则，怎么在更少的输入下，覆盖到更多的case，这个可能不是简单的模版替换就能覆盖的。"

测试流派中，也有一种是对所有的场景，场景和场景的交集，然后都毫无遗漏地梳理一遍，然后整理测试用例，去覆盖的方式。这个流派就需要很强的大局观，因为如果有漏掉的场景，可能就会有 bug 藏身。
通过大量的覆盖，来消除 bug 提升质量。这个流派需要的工作量也是挺大的。一般来说，在做新 feature 发布时，这个流派比较合适。而到了某个模块上线后质量不佳，需要重新梳理的时候，这块的重复工作量就很大。


> "比如梳理出之前 bug 多的地方，针对性的重构小模块，然后想办法验证这个模块的问题变少了”

再说一类人，他们是代码品味流。这个流派往往更倾向个人英雄主义。代码品味流的，他们的偏好简化架构，重构代码，来提升质量...
我发现不同人的喜好会不太一样，可能跟他们善长什么有关。我觉得王垠是这种流派的，他会认为自己的代码是足够简洁优雅的，以至于所有 bug 都无处可藏。(其实我也是倾向这个流派)
一般代码品流的人编码能力会比较强，写代码时也比较有节操，但是总体的质量并不一定能得到保证。这里面有个问题是，复杂软件没法靠单个人写代码，也没法靠个人代码质量高出 bug 少来解决。
写个B树这种模块级的东西还可以，但是写个数据库这种，完全靠代码品味去把控质量就抓瞎了。

> "重构是手段不是目的呀...如果不能有指标向上面证明重构过后质量变好了...那上头会得出“没啥产出”的结论的"

对，在上层也很难得到支持：看到不好的代码，代码品味流会倾向于重构代码，于是，上面问，做了什么，重构代码。有什么产出？没什么产出。因为 bug 被无声无息地解掉，就好像它从来就没有存在过。
重构代码解 bug，算是一种匪夷所思的方式，不好量化。

> "用量化困难的案例说明, 转接覆盖率不够的矛盾到不好进行测试覆盖, 所以想要先覆盖得先重构, 有理有据"


所以，你是哪个流派呢？