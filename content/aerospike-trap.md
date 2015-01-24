aerospike这一路的坑

开始尝试着在实际项目中使用aerospike了。这一路下来，踩坑还真不少。总体来说aerospike的成熟度相比起redis还是差了好多的。

首先遇到的是内存使用到2G崩溃问题。我在一台4G内存100G的SSSD笔记本上做测试。观察到的问题是每次写到2G内存时就会崩溃。去给他们提了issue，后面发现是他们代码中内存分配器的实现每次申请1G的内存，分配失败后直接崩溃。在低配机器上是硬伤。

然后是overcommit_memory参数，[前一篇文章](overcommit.md)写了这个问题。这是一个虚拟内存配置相关的参数，设置为2后，内存不足线程创建会失败。导致aerospike服务器起不来。

有些功能还是挺弱的，必须写udf实现。比如对于使用key map的结构，原子自增就没有默认提供，批量查也没有。

aerospike是不存原始的key的，用户的key会被hash成一个20字节的digest作为内部的key使用，能拿到的也就是这个20字节的digest了。这样有什么问题呢？首先是会有冲突导致覆盖。由于是使用hash(rawkey)作为它内部的key使用，而原始的key不存，那么不管概率多少冲突肯定是存在的。比如hash一个abc得到xxxxxx，而hash一个bbf也得到一个xxxxxx，那么后面写的数据就会把前面的那个覆盖掉！这个在数据比较重要的应用场合是不可接受的。其次是数据无法导出。这里指的是导出到原始的key-value数据，因为key是没有存的。处理办法可以自己设置一个bin把原始的key写进去。

网络配置方面，aerospike会局域网广播心跳同步信息，有multicast和unicast两种方式方式。unicast会维护任意两台机器之间的连接，所以会有n^2的连接的开销，官方更推荐使用multicast。但是使用中multicast不太好用。当时同事也开了一个实例，然后我这边服务就起不来了。

使用aerospike的Go客户端遇到的bug，返回一个Error，却没有error信息。跟代码发现是有部分的result code没实现。去提issue却发现就在几天前已经有人提过这个问题了。

做性能测试时，发现goroutine越多延迟越长，qps越低，吞吐量是固定的。怀疑到Go客户端库实现问题或者网络问题，最后发现是GOMAXPROCS没设置。这个是自己坑自己了。这里有个很坑爹的地方，Go的客户端库本身是没有设置GOMAXPROCS的，而Go客户端附带的tools中包括benchmark等是在代码中设置了GOMAXPROCS的。

afterbunner是一定要用的。这个脚本隐藏得很好，在/opt/aerospike/bin/afterbunner中，官方文档很少提及。这个是自动根据机器的配置生成配置文件。不配置好根本没法发挥出机器的性能来，而手动去配置很多项根本不知道该如何设置。afterbunner脚本就解决了这个问题。

先写这些吧，更多的问题还等着去踩。总体来说，坏的方面是比较小众，成熟度和资料都不如现有的一些nosql数据库。好的方面是aerospike性能真的很好，在高QPS环境中还能保证很低的延迟这是非常难道的，也正是我们应用场景需要的。

---------2014.10.16更新-----

存储原始key是可以的，Key结构体中有个useKey，将WritePolicy.sendKey设置为true，则useKey将被存储到一个bin中。

Go客户端中Record过期时间用一个int表示的，实际上就是int32的(目前版本已fix)

---------2014.10.29更新-----

udf那边，传入的lua的变长参数不能过多，否则会导致服务崩溃。具体见我的提问：[http://discuss.aerospike.com/t/my-udf-make-aerospike-crash/462](http://discuss.aerospike.com/t/my-udf-make-aerospike-crash/462)

---------2014.1.14更新------

默认的evict/expired线程工作周期是120秒，不合理。这个线程周期性地扫描过期数据以及回收垃圾。如果数据量很大，扫描需要的时间会超过120秒。扫描周期只有120秒，那么这个线程就会永远工作不会停下，对性能影响很大。具体解决方法是根据需要去设置`nsup_period`参数。