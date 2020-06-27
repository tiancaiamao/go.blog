如果一个 process 请求资源时 block 了，它需要发送 probe 消息到资源的所有者。

probe 消息包含：被 block 的 id，消息发送者 id，发送到的目标 id。对于最初的 probe 消息，被 block 的 id 和发送者的 id 是相同的。

收到 probe 消息，将被 block 的 id 加入到自己的列表里面。

收到 probe 消息时，如果自身是在 waiting 状态的，则要转发这条 probe 给更上游，知道更多 process 阻塞于它了。

这样的 probe 过程可以让每个 process 知道，谁被自己阻塞了。

如果收到一条 probe 消息，消息中的被阻塞 id 已经包含在自己的 waiting 队列中了，就说明检测到成环了，也就是死锁了。
