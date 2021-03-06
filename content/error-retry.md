这里说的错误处理是指分布式场景里面的错误处理。分布式场景下，结果是有三态的，成功，失败，不可知。
失败和不可知很容易被混一谈，它们都被当作错误，但其实是完全不同的：失败的结果是确定的，操作没成功，并且不会产生副作用。不可知的结果不是确定的，有可能操作成功了。这时如果重试，就有可能导致数据被写了多次。

先说几个场景

proxy 的自动重试。假设 proxy 有两个功能，一个是负载均衡，另一个是自动重连。假设 proxy 作用在业务层转发，则自动重连在有些场景下面可能会出问题。比如说 proxy 接的是一个分布式存储，一条写操作下去，该节点可能执行完之后 down 掉了。此时结果是不可知，有可能是成功。那 proxy 换一个节点重连，客户端把写操作再执行一遍，就可能重复插数据了。

closing 状态的错误处理。这是一个需要特别小心的状态。一个后台服务，收到服务关闭信号，跟断开网络监听端口，可能不是处于同一个原子操作内。这时会发生的事情是，从服务收到关闭信号到完全关闭期间，处于一个 closing 状态。这个状态下可能仍然会收到请求，这些 inflight 的请求，是有可能被执行成功的。对于客户端来说，它会收到一条服务器关闭的错误，这个错误其实是不可知，而不是失败。假设客户端重试这条请求，就有可能出问题。这就意味着，错误处理是客户端和服务端共同的事情，客户端的错误处理必须能区分失败和不可知。很遗憾，对于 SQL 协议，这种场景客户端无法区分。

如何设计一个可重试的协议呢？我们可以在服务端记一个 id 号，将这个 id 号返回客户端。当错误类型是不可知的时候，协议要约定让客户端拿这个 id 去服务端查询状态，是成功或失败。
tcp 是一个典型的例子，它就自动重试了更底层的错误。

tcp 本质上是一个只有 2 个 peer 参与的分布式共识协议。send 和 receiver 之间有三次握手，四次分手，并且对包的组装，重传，流控，等等达成共识，是一个有状态的协议。

我们得出在不可知状态下错误处理，重试的条件：

* 操作本身是幂等操作。即使被执行多次，结果也是一样的。
* 另一种是通讯的两端都记状态并对比。

第一点很好理解，比如说读操作就是幂等的。再比如说 kv 存储，把同一个 "key = value" 写操作执行多遍也是幂等的，当然这里涉及到一个时序问题，前提是有 MVCC 条件下。
第二点其实就是业务感知，通过协议层的约定，把不可知状态给干掉了。

分层

分布式存储就不能 proxy 么？当然不是的。假设提供服务是在 SQL 层，而 proxy 在 tcp 层，比如多台 tidb + lvs。因为分层了，下一层里面发生的错误，对上层透明。

如果我们把 tidb 的网络消息栈从上到下看一遍，它也是一个分层的：

    transaction mvcc multi-raft grpc ... tcp

tcp 提供了一个可靠的基于字节流的协议。grpc 在底层网络协议之上，构建了一个可靠的基于请求包的协议。

**不同的分层应该有各自的抽象。上层不应该去 assert 很下层的实现细节**，合理分层之后，我们说每一层的错误处理。

比如事务冲突，这是属于业务层关心的。

rpc 是更下一层。这一层应该屏蔽非业务的细节。比如 tikv 挂了，正在关闭，这都是它能处理的。消息队列满了，server is busy，也丢在 rpc 层处理了。如果要拆细，其实属于更上面一点点，就像网络模型可以拆四层或者七层。

multi-raft 这里，要处理 region 分裂合并，请求应该路由到哪个 region，重试 region 分裂之类造成的失败。

就像 tcp 的报文格式一样，在设计 grpc 的消息包时，也要分层，要考虑每一层的错误，并且不要跨层处理，尤其是不要混。比如说把一个 raft 层的错误设置成 other error，一直上抛到 transaction 层，那 transaction 层对这个 error 是懵逼的。


2PC 的 commit 阶段，如果发生了网络错误，这是一个 undetermine 的状态。但是 rpc 层并不知道，也不应该知道业务逻辑执行的是不是 2PC 的 commit。
multi raft 也不知道事务是否执行的 2PC 的 commit 操作，所以这种错误应该由 transaction 层处理。

这里暴露了一个矛盾的地方，这个矛盾大概是错误处理麻烦的根源：

    下层 callee 并不知道它自己是否应该被 retry，因为它不知道它被调用的语境
    上层 caller 并不知道被 callee 会返回什么错误，错误类型太多了，assert 每一种错误不现实


第一个问题，我们可以约定：callee 返回错误，caller 根据语境决定是否 retry。

第二个问题，我们合理分层，不要把太下层的错误抛到上层

错误并不描述它是否 retryable，因为 callee 并不知道错误是否 retryable 的。
closing 状态错误也是一个 undetermine。

总结一下:

* 分布式环境下，遇到结果不可知的场景不能轻易重试
* 合理分层，每一层处理自己可处理错误，并且向上层屏蔽细节
* caller 和 callee 之间约定好错误处理方式
