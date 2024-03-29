天之炼狱开源[项目](https://github.com/opendarkeden/server) 把代码放出去有好一阵了，都是不温不火，准确说是无人问津的状态。
其实只是把代码公开，这种不叫开源。开源一定是能形成一个社区，由社区的力量去推动项目的发展。

之前我想，怎么样降低门槛，让人很方便地可以搭建一个[单机版](https://github.com/opendarkeden/client/blob/master/standalone_version.md)，本地就能玩游戏。为此做了不少的工作，一直简化到 docker 几乎一行命令可以启动服务器的程度:


```
docker-compose -f odk.yml up -d
docker exec -it docker_odk-server_1 ./start.sh
```

结果发现这样都门槛太高，99% 的网民/玩家是连安装 docker 都搞不定的。所以我想，要不要搞一个体验版，不用安装服务端就可以玩起来。
但是我没有外网，怎么样把一个内部服务器暴露出去呢？然后就研究了一下这个问题，于是有了这篇博客。


一开始我想到的是花生壳之类的，网上搜了一下文章发现它是通过外网服务器做"转发"的。转发这个就不太好了，转发多一跳会增加网络延迟，并且这类服务它可能会有转发的流量限制。最好是能做成 p2p 的，游戏的客户端和服务端都在内网的情况下，还能够通信。

这个涉及的一个技巧叫做 "打洞"，或者 "UDP 打洞"。需要有一个外网的服务器，协助两个内网 peer 去建立连接。假设 A 要连接到 B，通过外网的中间人 C，过程是这样子的：

1. A 向 C 发送 UDP 请求
2. C 收到消息后，可以记录下 A 的外网 IP
3. B 向 C 发送 UDP 请求
4. C 收到消息后，可以记录下 B 的外网 IP
5. C 把 B 和 A 的外网 IP 分别返回给 A 和 B，于是 A B 可以知道彼此的外网 IP
6. 但是... 这时 A 向 B 的外网 IP 发 UDP 请求，B 是收不到的
7. 需要 B 先向 A 的 IP 发送 UDP 请求 (这个请求会被丢弃)
8. 然后 A 再向 B 的外网 IP 发送 UDP 请求，这时就可以成功建立连接了

打洞的关键点是在第 7 步， A 想连接 B，需要 B 先 A 发起请求。
原理是这样子，A 向 C 完成 UDP 时，建立了一个 A(192.168.0.xx:port1) -> 网关(121.60.64.14:port2) ->  C 的映射，凡是到网关的 IP:PORT2 的，网关都知道消息包是回给 A 的 内网IP:PORT1 的。同理，B 到 C 也是建立了一个这样的映射路径。

B 向 A 发消息的时候，由于消息发送方的 source 不是 C 而是 B，A 的网关会丢弃这个消息包。但是这个过程在 B 的网关那边建立好了映射信息，**凡是从 B 的网关的 XX 端口出去的消息，都是 B 给 A 的； 所以，凡是从 A 过来的到 xx 端口的，都可以转发到内网的 B**。这就完成了打洞，A 可以连接上 B 了。


利用 UDP 打洞的这个机制，可以让两个内网进程相互联系到对方。

然后我想，这得改游戏服务端啊，而且游戏服务端用的不是 UDP，还挺麻烦的。RFC5389 是做这个的，Session Traversal Utilities for NAT，标准化的解决方案，处理内网环境中穿透 NAT 的协议。对我想解决的问题来说，还是太重量了一点。

灵光一闪，要不搞个代理吧！假设在游戏的客户端那边，有一个 client proxy，在游戏服务器那端，有一个 server proxy，然后两者形成了一个 tunnel。
kcp 正好是基于 udp 的，打洞就很方便，而且它提供了对上层的可靠传输，如此甚好。


```
game client -> [client proxy] <-- kcp --> [server proxy] -> game server
```

这个就等价于

```
game client -> game server
```

client proxy 和 server proxy 都分别是和 game client 和 game server 同机器上部署的，所以这个转发造成的延迟影响可以基本忽略，不会向中间服务器流量转发那种多出一跳的网络延迟。

- client proxy: 伪装成 server 的样子，实则是把连接到自己的 conn forward 到真实的 server 那边去
- server proxy: 伪装成 client 的样子去连接 server，实则是代理从 kcp 过来的流量

同时，proxy 还要负责 kcp tunnel 的打通。kcptun 是一个类似项目，不过它没有处理打洞相关的事情。

处理流程是 client proxy 监听 127.0.0.1:9998 (伪装成 server 的样子)。client proxy 收到请求后，打通到 server proxy 的 kcp 连接。
具体建立连接的过程，关键点见上面打洞流程部分。

tunnel 打通以后，server proxy 监听 kcp 连接。收到连接后，去连接真正的 :9998 游戏服务器端口。

为什么不直接 client 连 server 呢? 因为 server 没有外部 IP。不要 server proxy，只使用 client proxy 是否可行? 不行，因为 server 本身是没有 kcp 通信的能力的

一部分是打洞，一部分是 tunnel。

我在网上随便找了一个类似的项目，然后改了改，结果就是这个东西了 https://github.com/opendarkeden/tunnel
