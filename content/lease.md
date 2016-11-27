lease，即租约，是个强大的东西。

## lease用于缓存

先看缓存的lease。假设每个client都去server取数据，server可能成为性能瓶颈。然后就有了缓存，client每看看自己有没有相应的缓存，没有才去找server要。但是….有缓存就有server和client的不一致了。比如client A向server发送消息修改了某个东西，然后client B上的缓存就是过期的了。

lease如何解决缓存问题呢？ server给client B缓存颁发一个lease，在lease到期之间，server承诺client B的缓存是有效的。这里client A发消息要求修改，server收到消息后不会执行A的请求，直到lease过期。

lease是很强大的，比如server和B的网络故障了，server可以一直给B重复发那个lease，只要有一次成功，lease都可以生效。如果B一直没收到lease，它就不会使用本地缓存，不影响正确性。

当然，由于lease是按时间点约定的，这就要求各个机器上的时间的一致性。这个不具体讲，不是讨论重点。

## lease的本质

上面缓存中的lease机制只是其应用之一。lease的本质是什么呢？

lease本质是颁发者对接受者的一个承诺。在缓存的例子中这个承诺是“在lease有效期内缓存不会过期”。其实这个承诺可以是任何东西，不一定非得是缓存有效时间。

这就非常强大了。 首先，同一个承诺我可以承诺无限多遍。这就是分布式环境中允许消息重复。
然后，这是一个单向的承诺，不要求一定有应答。这就可以解决状态问题。
插讲一下状态问题。分布式系统中是无法确定另一个结点状态的。像TCP三次握手那种机制也不行，我发给你，你应答，我再对应答的确定。谁能保证机器不是把应答确定包刚发出去机器就挂了呢？？ 然后另一边收到应答确认包以为那台机器还是好的，这就是状态问题。所以说，分布式系统中是无法确定另一个结点状态的。

第三，承诺可以是任何东西这一点，让lease可以做很多种事情，可用性大大提高。

## lease用于确定状态

先看一个例子。假设有一个master，然后有多个副本。副本中有一个是primiary，其它都是secondary。master用于管理谁是primiary。假设我们用常规的心跳机制，就会出现状态问题。

比如，primiary其实是可以正常工作并提供对client服务的，primiary与其它secondary之间也是OK的，但master和primiary之间连接闪断了。这时master收不到primiary的心跳，就认为primiary挂了，然后重新选择primiary，这样系统中就会出现两个primiary，也就是双主问题。 其实最基本的原因还是单纯用心跳机制master是无法确定结点状态的。

下面就谈lease用于解决结点状态问题。

引用lease机制，master向primary颁发lease，承诺“在这个lease有效期内我不会重新选取primiary”。primiary每个心跳包过来，master就可以颁发新的lease把有效期延长。如果过了lease而master还没收到心跳包，它就认为primiary挂了并重新选择。

这时考虑primiary的两种情况：
第一种，primiary是真的挂了，所以没给master回复。这时重选当然是合理的。
第二种，primiary没挂，只是和master通信出问题了。这时primiary知道自己的lease到期，master肯定在选取新primiary了，于是它就会放弃primiary身份，不会出现双主问题了。

还有一些其它应用。反正lease的机制是很强大的。
工程上lease的有效期一般选择为10秒。
