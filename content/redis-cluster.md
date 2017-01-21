读了一下[redis集群规范](http://redis.io/topics/cluster-spec)，记录下一些点。

## 设计的主要和特性

### redis集群目标

* 高性能，线性scalable到1000节点级别。无proxy。
* 对于写的保证：尽最大努力交付。意味着即使通过客户端写成功了，但实际上写操作可能丢失。
* 可用性："大多数master节点可达，并且每个不可达的master节点至少有一个slave可达"

### 实现了的子集

所有的单key命令都是可用的。复杂的多key操作如果属于同一个节点，也是实现了的。

实现了hash tags的概念，可以将特定的key强制存储在相同结点上面。但是手动reshard期间，多key操作可能不可用，而单key操作一直是可用的。

redis集群不支持多数据库。也不允许select命令。

### 集群协议中客户端和服务器的角色

集群节点负责存储数据以及交换集群状态，包括将key映射到正确的结点。自动发现其它节点。检测不工作的节点。必要是会将slave提升为master。

集群节点使用TCP，二进制协议，Redis Cluster Bus。每个节点会连到其它所有节点。gossip协议。处理结点发现，ping，通知一些特殊条件，pubsub传递，手动failover。

不代理，所以会返回MOVED和-ASK消息。客户端不知道集群状态也能工作。

### 写安全性

由于是异步写副本，master挂掉后，最终状态为顶上来的那个slave中的数据。意味着，一段时间窗口内的写可能丢失。

举两个写不一致的例子。
1.master收到client写请求，master回复client写ok了。但是master还没把这个写同步到slave中，master挂掉了。slave提升为master。该写请求丢失。
2.master暂时不可达了。slave提升为master。原来的master又可达了(但身份还没切换到slave)，client的路由还没更新过来，继续往它里面写数据。写请求会丢失。

要检测到master挂掉，需要超过半数的master都发现它不可达，并且持续至少NODE_TIMEOUT。

### 可用性

假设集群分区割裂了。那么minority那部分的分区将不可用。而majority端那边，“大多数master节点可达，并且每个不可达的master节点至少有一个slave可达”，那么在NODE_TIMEOUT时间加上选举新的master这段时间之内都是available的。

### 性能

嗯，单机N倍！

### 为什么不做merge操作

redis集群设计不记录数据版本，主要考虑是value通常很大，比如说list或者set，还有就是数据类型很复杂。说白了就是版本不好做，数据不好合并。

其实也不算技术limit啦，CRDT是可以做到的。不过那样就不符合redis集群的设计了。

## redis集群组件综述

### key的分布模型

key被hash到16384个slot。正常情况下每个slot只由一个节点负责(不算slave)。

	HASH_SLOT = CRC16(key) mod 16384

### key hash tags

对hash slot的计算有一个例外就是hash tags。hash tags是一种确保多个key分配到同一个hash slot的方式，用来实现集群的多key操作。

如果key中包含一个"{...}"模式的子串，只有{}之间的子串会被hash于计算slot。使用第一个出现的{}

* 对{user1000}，user1000将被用于计算hash slot
* 对foo{}{bar}，使用整个串，因为第一个{}里面是空的
* foo{{bar}}zap，使用{bar，因为它是在第一个{}内面
* foo{bar}{zap}，使用bar

### 集群节点属性

集群内每个节点都有一个唯一的名字。第一次从/dev/urandom读，后面都不会变。除非改配置文件或者使用CLUSTER RESET命令。

节点ID是用于识别集群内的节点的。可能IP会变，但是节点ID不会，集群会识别出IP/端口的变化并通过gossip协议重新配置。

cluster nodes命令可以打印当前集群的节点信息。

### 集群总线

集群中每个节点都监听一个额外的TCP端口用于接受来自其它redis节点的连接。这个端口是正常服务端口加10000，如果redis端口是6379，那么用于集群总线的端口就是16379。

### 集群拓扑

redis集群是一个每个节点都和其它所有节点连接的网格。如果有N个节点，那么每个节点都有N-1条往外的TCP连接，以及N-1条输入的连接。并且是keepalive而不是按需创建的。

尽管集群节点形成了一个完整的网格，节点之间是使用gossip协议和配置更新技术来避免节点间过多的消息通信，这样消息数不会成指数增长。

### 节点握手

节点总是在cluster bus端口接受新连接。如果收到ping就会回复，但是如果收到(除了ping以外)不属于集群节点的包，会将它丢弃。

有两种方式节点会接受其它节点成为cluster一部分：

* 如果节点发一个MEET消息。MEET消息跟PING消息很类似，但是会把节点当作属于集群。只有系统管理员使用下面命令时，节点会发送MEET消息：
  
  CLUSTER MEET ip port

* 如果节点已经通过gossip协议成为一个受信任的节点。即，如果A知道B，B知道C，最终B会给A发送关于C的gossip消息。这里，A会注册C成为网络的一部分，并尝试连接C。

这个机制保证了最终节点可以知道其它节点，并且阻止了在改IP或者一些其它事件时redis集群可能弄混。

## 重定向和resharding

### MOVED重定向

client可以随意给集群任意节点发请求，包括slave。节点会分析请求，如果hash slot是这个节点负责的，那就简单的处理这个查询，否则节点会检查映射表，并返回MOVED错误，如下所示：

	GET X
	-MOVED 3999 127.0.0.1:6381

client需要将请求重新发到指定节点。如果在client等了很久才重发消息，期间集群配置变动了，目标节点会回复MOVED。client联系到过时的节点都是这种情况。

尽管集群中是用ID来作为节点标识的，这里跟client通过都是简单的使用IP:port。

虽然不强制要求，但是client要记下slot 3999是由127.0.0.1:6381提供的。这样新的命令就会挑选正确的节点发送了。

还有一种做法是client在收到MOVED的时候，使用CLUSTER NODES和CLUSTER SLOTS命令，因为遇到重定向时，很可能许多个slot都变化了，因此client尽快更新配置可能是最后的策略。

client还必须能够处理-ASK重定向，不然不能算完整的client。

### 集群在线改配置

redis集群支持在运行时添加和删除节点。实际上添加和删除节点都是抽象成同一种操作，将hash slot从一个节点迁移到其它。

核心部分就是移动hash slot的能力。实际上hash slot就是一系列的key，因此resharding就是将一些key从一个实例移到其它实例。

手动迁移命令：

```
CLUSTER ADDSLOTS slot1 [slot2] ... [slotN]
CLUSTER DELSLOTS slot1 [slot2] ... [slotN]
CLUSTER SETSLOT slot NODE node
CLUSTER SETSLOT slot MIGRATING node
CLUSTER SETSLOT slot IMPORTING node
```

前两个是简单的将slot赋到redis节点。ADDSLOTS通常是创建新集群的时候用。DELSLOTS主要用于手动修改集群配置或者调试，实际上很少使用。

SETSLOT slot NODE形式的子命令用于将一个slot赋给特定的节点ID。MIGRATING和IMPORTING用于迁移hash slot从一个节点到其它节点。

* 如果一个slot状态是MIGRATING，如果请求的key存在，节点会接受查询请求，否则会返回一个-ASK重定向。
* 如果一个slot的状态是IMPORTING，节点会接受所有的查询请求，但是只处理ASKING命令，如果不是ASKING命令，会返回-MOVED重定向。

### ASK重定向

为什么是ASK而不是MOVED? ASK意味着，只是下一条命令要查询特定的节点，而MOVED是后面的查询都是到了其它节点。

我们要限定client的行为。因此，IMPORTING的节点只接受ASKING命令。

从client的角度，ASK重定向的完整语义：

* 如果收到ASK重定向，只是下一次把请求发到特定节点，之后仍然是查询老的节点。
* 使用ASKING命令启动重定向查询。
* 暂时不要更新本地的hash slot

### 客户端首次连接和重定向处理

client要尽量聪明一些，记下slot的配置。不过不需要是最新的，因为连到错的节点后会收到重定向。

通常在以下两种情况，client要拿一份完整的slot到节点映射表。

* 启动的时候
* 收到MOVED重定向时

CLUSTER SLOTS命令可以拿到相关信息。

### 多key操作

使用hash tags可以很容易做多key操作。

但是reshard的时候，多key操作将不可用。因为迁移的时候数据分散在两个节点之间了。这个时候会返回一个-TRYAGAIN错误。

### 利用slave节点提升读性能

正常情况下，slave节点会将client重定向到master。但是可以用READONLY命令，从slave读。

在只读模式下，只有slave的master不拥有对应的slot的时候才会发重定向。

## 容错

### 节点心跳和gossip消息

集群节点会持续交换ping和pong。这两类消息结构相同，都带了配置信息。实际上只有type字段有区别。统一都叫心跳包。

通常是发ping然后返回pong。不过也可以直接发pong，这样就不需要返回，可以尽快把配置信息广播出去。

通常ping会随机挑选出一些节点，这样每个节点的发送消息是常量，跟集群规模无关。但是要确保，每个节点在NODE_TIMEOUT/2时间之内，会ping到所有的其它节点。

### 心跳包内容

### 异常检测

如果多数节点都不可访问某个master或者slave节点，slave会被提升为master，如果不行，集群将进入error状态并停止接受client请求。

每个节点都维护了一个它知道的节点的列表，有两个flag用于异常检测。PFAIL和FAIL。PFAIL意思是Possible failure，并不确认异常类型。FAIL意思是节点挂了并且大部分的master都确认过。

*PFAIL标记*

如果一个节点超过了NODE_TIMEOUT无法连到另一个节点，它会将这个节点加上PFAIL标记。master和slave都可以标记其它节点为PFAIL。

不可达的概念是发了一个ping，超过NODE_TIMEOUT还没收到响应。为了增加正常情况下的可靠性，在超过NODE_TIMNEOUT一半时间后，会尝试重连节点。这样子可以确保连接是keep alive的并且连接坏了不会导致错误的报告节点挂掉。

*FAIL标记*

PFLAG只是本地信息，并且不足以触发slave提升。PFAIL变为FAIL后才能确认节点挂了。

A节点中标记了B为PFAIL。A通过gossip收集到其它节点关于B的信息。多数master标记了PFAIL。那么A会将B标记为FAIL，并给所有可达的节点发FAIL消息。

变成FAIL需要经历PFAIL，清除FAIL有两种情况：

* 节点可达了，并且它是slave。直接清除FAIL标记，因为slave不需要容错。
* 节点可达了，它是master但是不对应任何slot。也可以直接清除FAIL标记
* 节点可达了，它是master，并且过了N倍的NODE_TIMEOUT时间没有检测到slave提升。这种情况有利于节点重新加入到集群。

## 配置处理，传递和容错

### 集群当前epoch

redis集群用了一个类似raft算法中"term"的概念，在redis集群中叫做epoch。使用它是为了给事件一个递增的版本，这样，当多个节点提供的信息冲突了，其它节点可以知道哪个状态是最新的。

集群创建的时候，所有节点，包含master和slave，都将currentEpoch设置为0。每次收到其它节点发来的包，如果发送者的epoch大于本地，则将currentEpoch更新到发送者的epoch。

### slave选举和提升

slave选举和提升是由slave节点处理的，master节点会投票。当master处理FAIL状态，并且至少有一个满足条件的slave时，会执行slave选举。

------------------

...后面部分写不怎么好懂了...

但是后面这一块才是最有价值的关于分布式的一些东西，下次继续读完。

