# 1. 套接字API

## connect

如果是TCP协议的话，客户端调用connect会开始进行三次握手。从TCP状态迁移图可以看到，如果connect之后会进入SYN_SENT状态， 如果失败的话那么是不可用的，必须首先close然后重新socket.

默认情况下connect会阻塞到三次握手完成，也就是对端ACK响应之后返回。但是如果是设置了非阻塞呢？那么会返回EINPROGRESS但三次握手依然进行，通常要使用IO复用进行监听。

如果SYN分节发给一个主机存在，但是主机端口并没有提供服务，会收到RST分节，那么connect会返回ECONNREFUSED的错误。 

如果SYN分节发给一个在路由器中存在表项，但是已经没有运行主机的话，没有得到SYN的ACK分节，就会返回ETIMEDOUT的错误。

那么，是否可能通过这两种返回值的不同来判断一个主机的存在？答案是不行，通常iptables做了设置，ETIMEDOUT很可能是主机存在，但是SYN被Drop掉了。

如果SYN分节发给一个不在路由器表中的表项的话，那么就回返回ENETUNREACH的错误。路由器会返回ICMP错误"destination unreachable"，然后客户端内核接收到之后，依然会尝试继续发送SYN分节，直到超过一定次数之后就会停止然后返回ENERUNREACH错误。但是也有另外两种情况，一种是按照本地系统转发表根本没有到达远程路径，那么就会直接返回错误，另外一种情况是connect不等待完成就返回。

如果返回EADDRINUSE，发生这种情况通常是本地可用的port都使用完。可以通过下面这些办法解决：

* 修改本地可用的port数量
* 加快回收处于TIME\_WAIT连接
* 安全复用处于TIME\_WAIT连接

## bind

一般是在服务端使用，客户端调用connect会自动的分配端口进行绑定。

如果IP选择通配地址的话，那么内核自己选择IP地址，否则就是进程指定。对于port，如果port==0的话，那么内核选择port,否则是进程指定port。

## listen

listen函数将主动套接字设置成为被动套接字，指示内核应该接受指向该套接字的连接请求。对于TCP状态转换来说，是从CLOSED状态变成LISTEN状态。

    int listen(int sockfd, int backlog);

注意到listen有两参数，第二个参数backlog是干什么的呢？

从收到SYN分节，到被accept进行处理 这段过程中连接上的客户端，服务端是需要保存这些连接的。服务端需要将这些连接保存在一个队列内部，这个队列内部包括的连接状态分别是：

* 接收到SYN分节，返回了ACK分节和自己的SYN分节。连接处于SYN_RCVD状态。incomplete queue
* 三次握手完成，等到accept进行处理。连接处理ESTABLELISHED状态。complete queue

在队列里面并没有分配fd，只是保存了连接的状态，直到accept之后才正式分配fd。至于这个队列的长度，通常我们称为backlog。在linux下面如果我们man listen会发现如下说明：

>>>The  behavior  of  the  backlog argument on TCP sockets changed with Linux 2.2.  Now it specifies the queue length for completely established sockets waiting to be accepted, instead of the number of incomplete connection requests.

也就是说，在linux系统下面，backlog的值修改成为了已经完成三次握手但没有accept的队列长度(complete queue)。 而不是SYN_RCVD队列长度(incomplete queue)

如果客户端SYN分节到达的时候，服务端incomplete queue已经满了，会怎么处理？是否应该返回RST分节还是不回复ACK分节，而让客户端进行重传。两种都可以接受，实际上让客户端重传更好一些，因为如果响应RST分节的话那么客户端没有办法区分，是因为服务端没有开启对应的服务，还是因为incomplete queue已满这两种情况。客户端可能稍微重传等待一段时间，服务端incomplete queue部分连接应经建立起来了，客户端就可以被处理了。

## accept

默认情况下面accept是阻塞版本，直到连接三次握手完成进入complete queue并且被取出才返回。如果我们使用非阻塞版本的话，那么accept就会立刻返回。和非阻塞情况一样我们也必须处理accept立刻返回的情况，如果没有可用连接的话返回EWOULDBLOCK/EAGAIN错误。之后可以使用IO复用来检测accept是否有新连接，如果有新连接的话那么fd变成可读状态。非阻塞和阻塞accept相同，也必须考虑ECONNABORTED这样的错误。

什么时候会ECONNABORTED呢？

建立连接完成之后但是在accept之前，如果客户端取消连接发送RST分节的话，那么accept得到的就不是一个有效套接字了。系统可以选择在accept内部完成这个操作，也可以返回错误交给用户来完成。POSIX规定是返回ECONNABORTED错误(BSD返回EPROTO)，然后在应用层上可以进行忽略然后进行下一次accept。

假设连接断开后，我们继续读写，会发生什么事情呢？读取会返回-1，错误是reset by peer，而写会提示broken pipe，错误码是EPIPE，通常还会触发SIGPIPE，这个信号默认动作是使进程退出。

## close

close与shutdown的区别是，close会做引用计数，而shutdown是close引用计数==0的时候的真实操作(SHUT\_RDWR)。并且可以看到close是全关闭，而shutdown可以完成半关闭。SHUT\_RDWR就相当于调用一次SHUT\_RD和SHUT\_WR。

SHUT_RD能够关闭读半部，执行这个部分不会发送任何分节，而kernel内部会将已经接收到的所有数据都全部丢弃，继续read这个fd都是返回0。如果对端继续发送数据的话都会被无条件地确认。

SHUT_WR能够关闭写半部，执行这个部分会发送FIN，而原来kernel内部维持的数据会首先全部发送出去，继续write这个fd的话会产生EPIPE错误。

## write

应用层只是调用write将应用层数据完全copy到内核的tcp send buffer上，至于这个tcp send buffer大小是SO\_SNDBUF来控制的。

写入write成功仅仅表示写入到tcp send buffer而不表示已经发送或者是对端已经接收到。系统会将tcp send buffer按照MSS来进行切分，并且加上TCP的头部传递给IP层。因为TCP之前已经按照MSS进行了分片，那么在主机的IP层不会进行分片操作。tcp send buffer不会被丢弃直到对端收到这块buffer所包含内容的确认为止。

同时如果数据链路层输出队列满的话，那么新到的IP分组将会丢弃，并且沿着协议栈向上反馈。TCP注意到这个错误的话，那么会等待一段时间然后重传，而不会让应用层看到。

如果底层tcp send buffer空间不够的话，如果使用的是阻塞IO的话那么就会hang住直到tcp send buffer有空闲然后继续写入，直到数据完全写完为止。如果使用的是非阻塞IO的话，那么如果有部分空间的话就会返回已经成功写入的字节数，否则返回错误EAGAIN.

## io multiplexing

select存在的问题是

1. 1024的大小限制
2. 每次都需要重置，每次调用都需要拷贝参数(fd集合)从用户态到内核态
3. 每次调用select都需要在内核态遍历传进来的fd 

poll比select的接口好一些，表现在它是没有fd集合大小的限制，另外它不需要每次重新设置，因为事件状态都存在了revents这个字段上，而events是我们关心事件字段这个没有发生改变。

epoll比poll又多了一个epoll\_ctl函数，会在每次注册新的事件到epoll句柄中时（在epoll\_ctl中指定EPOLL\_CTL\_ADD），会把所有的fd拷贝进内核，而不是在epoll\_wait的时候重复拷贝。并且每个fd指定了一个回调函数，这样就可以不必要遍历了。设备就绪时，唤醒等待队列上的等待者时，就直接调用这个回调函数，而这个回调函数会把就绪fd加入到一个就绪链表。

# 2. 选项

## REUSEADDR

SO\_REUSEADDR选项允许监听的套接字绑定在一个正在使用的端口上。使用这个选项必须在 socket和bind之间调用。

进程退出后连接还没完全释放掉，重新运行进程绑定相同端口。会发生什么事呢？在bind的时候会出错，提示Address already in use。如果我们设置了REUSEADDR选项，那么bind不会出错。但是到connect，仍然是会出错的。

## NODELAY

开启本选项将禁止Nagel算法，默认情况下面是使用Nagel算法。使用Nagel算法效果是数据并不会立即发送，而是等待到一定大小的时候才会进行发送，这样可以减少分组。通过减少传输分组的数目，防止一个连接在任何时刻存在多个小分组确认。这点对于广域网网络环境非常合适，能够有效地利用网络。

减少网络拥塞。通常是因为网络上分组非常多造成的，一旦出现网络拥塞的话那么丢包概率就会上升。在广域网下面丢包重传代价是非常大的，所以都会尽可能地减少网络分组来提高在广域网下面传输效率。

如果只是在广域网情况下，考虑综合考虑网络情况并且有效利用的话，那么Nagel确实很好。但是如果从应用角度出发，如果是交互式应用程序，或者是涉及到局域网传输的话，那么Nagel并不适合。对于交互式应用程序希望尽快地响应，而在局域网内传输质量非常好，没有必要来减少分组的数目，相反快速和实时性才是最主要的。

## LINGER

默认情况下面，close的动作是发送完成缓冲区内数据，并且发送FIN分节之后立即返回。返回之后如果数据或者是FIN分节没有确认的话，那么tcp实现会自动进行重传，但是如果重传失败的话，我们也是没有办法知道的。

使用SO\_LINGER选项可以在一定程度上解决这个问题。SO\_LINGER使用的值是下面这个类型，如果l\_onoff=0的话，那么就是按照默认情况处理。下面我们讨论l\_onoff=1的情况。

    struct linger {
        int l_onoff;                /* Nonzero to linger on close.  */
        int l_linger;               /* Time to linger.  */
    };

如果l\_linger==0，那么close会立刻丢弃缓冲区内部数据并且发送RST分节断开连接立即返回，而不是走正常的断开连接过程。这样可以避免TIME_WAIT状态。但是实际上我们并不推荐这么使用，因为这样如果还没有发出数据的话都会被丢弃，而且对端会认为本端可能是因为状态出错等其他原因断开连接，而非主动断开。

如果l\_linger!=0的话，那么close会等待l\_linger(单位s)的时间或者是等待到最后数据和FIN的ACK返回为止。不过如果close设置为非阻塞的话，那么还是会立刻返回。如果close返回的原因，是因为等待到了最后的数据和FIN的ACK的话，那么返回值为0, 否则返回-1，errno为EWOULDBLOCK。

所以这里可以认为l\_linger是一个超时时间，在这么长时间内等待剩余数据和FIN的确认。这样的话，我们可以在一定程度了解到最后断开的情况。

## KEEPALIVE

客户端连接服务端之后如果客户端宕机的话，服务端不知道客户端已经宕机继续维持连接，我们称这种情况为半开连接(half-open connection)。如果服务端不检测出半开连接的话那么就会维持连接最终耗尽资源。当然在应用层服务端可自己来进行这个保活(keepalive)机制的实现，但是TCP内部也自带这样的机制。如果设置了这个选项的话，一段时间内套接字任一方向没有数据交换的话，那么TCP会自动给对端发送保持存活探测分节(keep-alive probe)，这个分节对端必须相应，结果会有三种情况：

* 响应ACK，对端存活
* 响应RST，对端可能已经崩溃重启，so_error置为ECONNRESET
* 对端没有响应，那么按照TCP重传机制重传，最终错误可能为ETIMEDOUT(超时)，ENETUNREACH(路由错误)

如果产生错误的话，如果我们使用IO复用/信号驱动IO的话，是可以立刻检测到的并且进行响应处理。

个人建议不要依赖这个特性，还是在业务层做keepalive比较合适。keeplive是一个2个小时的保活计时器，如果没有收到响应，每75s再重发一次，重试10次之后连接超时。

## TIMEOUT

SO\_RCVTIMEO/SO\_SNDTIMEO分别是针对套接字的读/写超时的设置，影响的函数包括：

    read/write
    readv/writev
    recv/send
    recvfrom/sendto
    recvmsg/sendmsg

设置超时的结构是struct timeval。

## SO\_RCVLOWAT

什么时候我们说IO是可读或者可写的？直观上，kernel的读的buffer有数据，我们就可以读，写的buffer还没满，IO就应该可以写。但是，不是这样子的。

可读或可写，是有参数控制的，就是SO\_RCVLOWAT/SO\_SNDLOWAT。尽管这两选项默认值是1。但是上必须大于SO\_RCVLOWAT，才算是IO可读，而不是大于0就算可读。

# 3. 协议相关

## MTU

网络MTU(maximum transmission unit，最大传输单元)表示对于这个网络来说一次传输的最大数据字节数(不包括网络封装占用字节数)，通常来说 MTU是网络硬件规定的。

对于我们最常用的以太网来说，MTU是1500字节。IPv4要求MTU至少68字节，这样可以容纳下20字节定长头+40字节可选部分 +8字节最小片段，而IPv6要求MTU至少1280字节。IPv4和IPv6都可以很顺利地运行在以太网上。

对于因特网来说我们底层使用的网络可能是异构的，在传输路径上某一部分使用的数据链路层是以太网，而在另外一部分使用的数据链路层比如是X.25的话，那么这个路径上面的MTU取决于最小的MTU,而这个MTU就是路径MTU(path MTU)。

当IP数据包传递到数据链路层的时候，会首先知道数据链路层的MTU并且按照数据链路层的MTU进行分片，这些分片在达到最终目的地之前是不会重组的。

TCP MSS(maximum segment size,最大分节大小),用于告诉TCP对端在每个分节中能够发送的最大TCP数据量。MSS的目的是告诉对端其重组缓冲区大小 的实际值，从而试图避免分片

## 状态机

![](http://dirlt.com/images/tcp-status-transmission.png)

## TIME_WAIT状态

这个状态是主动执行关闭的话会经历的状态，在这个状态停留时间是最长分节生命期(maximum segment liftime,MSL)的两倍，我们称为2MSL。MSL意思是任何一个IP数据报可能停留在网络中存活的最长时间，这个时间是一个有限值，不同系统设置不同。RFC建议值是2min，而BSD的传统实现是30s。

TIME_WAIT状态存在有两个理由：

* 可靠地实现TCP全双工连接终止
* 允许老的重复分组在网络中消失

首先看第一个理由。如果最后被动关闭方调用了close，发出FIN分节。active一端收到，但是响应ACK丢失了，那么被动关闭端还会重复发出FIN分节。如果这个时候没有TIME\_WAIT状态而是直接退出的话，当重发的FIN分节到达时，主动关闭端会直接响应RST分节造成连接的错误终止。

对于第二个理由，我们首先考虑一个迷途的重复分组(lost duplicate).如果A->B发送一个分节但是这个分节因为中途部分路由器出现问题， 在路由器停留时间过长，导致A->B发送分节超时而重发。如果A->B重发之后并且都关闭，然后AB又同时使用相同的IP和端口并且分节序列号 正好匹配的话(虽然概率很低)，那么这个以前连接的分组就会出现在新的连接被处理。而TCP_WAIT状态的话，使得不允许在2MSL之内使用相同的端口连接，就不会出现这样老分组出现在新连接上了。

## RST

产生RST分节通常有下面几个情况：

* 连接或者是发送到到某个没有监听对应端口的服务器上。内部产生一个ICMP端口不可达信息而TCP则使用复位。
* 想主动取消一个已有连接。通常来说我们是等待数据发送完成之后发送FIN称为有序释放(orderly release)，否则称为异常释放(abortive release)
* TCP接收到一个根本不存在该连接上的分组。通常这种情况是比如server掉电重启，而client认为连接还存在然后发送分组，这种情况称为
半打开连接(half-open connection)，server会以RST分节响应。

## 流量控制，拥塞控制

流量控制通过滑动窗口，主要由接收端控制，接收窗口就表示“我能接收多少”。

拥塞控制主要是四个算法：1）慢启动，2）拥塞避免，3）拥塞发生，4）快速恢复

慢启动是一个指数增加拥塞窗口。当它达到一个阈值一就执行加法增大(拥塞避免)。遇到拥塞发生之后，就执行乘法减小。

发送窗口的大小要取滑动窗口和拥塞窗口中的较小值。
