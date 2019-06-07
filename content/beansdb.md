htree是一个hash树的实现。注意其中存的值内容Item是

```
uint32_t pos;  //低8位表示bucket 高24位表示在文件中的位置 文件大小限制为2G
int32_t ver;  //版本号
char name[1]; //key用这个域存储
```

hash树的作用

* 第一是快速的 key 到 pos 的定位。这一点跟普通hash一样，有了pos就相当于有了路径和offset，可以找到数据块了。这个过程是一层一层进行的，每次根据key值，Node的层，定位到下一层的孩子
* 第二，冲突解决。用分裂这种方式来处理冲突的，节点满了就分裂到下一层。Item 全部在叶子结点。beansdb中是一个16叉树，每个Node有个Data指针，就是指向一个Item数组(变长的)。若 `node->count` 在于 `SPLIT_LIMIT` 就会分裂 node
* 第三，版本同步。这个是 hash 树的优点及在这里被使用的出发点。父亲块的 hash 值由孩子决定，如果两树的根的 hash 值相同 ，则是已经同步了，不需要更多比较。如果不一致，一层一层找下去 hash 不一致的层，很快找到不同步的数据块。

bitcast这个是日志结构存储的实现。核心技巧就是任一时间只有一个活跃数据，每次都是append方式添加，数据满了就新建一个bucket使之成为活跃。老的数据是只读的。日志结构存储的作用就是将随机写变成顺序写，用版本来控制过期数据。

* 增：版本等于1
* 删：版本等于-1
* 查：这个就没什么说的了
* 改：版本加1

数据是一个 DataRecord 结构，这个在 record.c 中实现.

bitcast中数据定位是依赖于htree完成的。或者说由 htree 得到 pos，pos=bucket+offset 即可定位到文件内容了

hstore是最后对存储的一个封装，依赖于 bitcast。主要完成的是对文件分配到对应 bitcast 调用的过程。

或者说hstore做的事情是：将 key 映射到 bitcast，再调用bitcast的功能完成剩下的工作。

其结构体中有个 `Bitcast **bitcasts`。逻辑上，hstore最多可以有256个bitcast。每个bitcast最多对应256个bucket。每个bucket是一个大小最多2G的文件

物理上，hstore 对应一个文件存储路径 path. 下面的目录结构是 path/0 ~ path/256 对应 bitcast.

两层就是 path/0/0 ~ path/0/256 ... 最多三层，最后在这些目录下的文件对应到bucket

举例一个查找流程：

1. `hs_get` 先对 key 做 hash，决定 value 是在哪个 bitcast。然后调用 bitcast 模块的 bc_get
2. `bc_get` 先调用 htree 模块的 `ht_get`，找到对应的 Item。
3. Item中有版本信息ver，位置信息 pos。pos 是 bucket 的 id 和偏移量拼成的一个uint32，于是得到了 bucket 和 offset

然后就可以读出数据了。

存储方面的核心部分到hstore就结束了。beansdb 的另一方面是 memcachedb 兼容接口，异步网络I/O

在 beansdb.c 中可以看到其处理逻辑。这里只稍微提一下，出于完整考虑。

main函数中，先是getopt进行参数处理。然后各种初始化，以及注册信号处理函数

最后到 `loop_run` 创建多个线程后到 `worker_main`。这里用到的epoll机制不懂，只知道个大概的思路。

有事件到达后，会通知到 `drive_machine`。函数参数是个 conn 的状态机，处理各种情况

源代码中用了好多别人的开源的文件，像 ae_epoll.c 这个是 epool 事件相关的，quicklz.c 是一个快速压缩相关的
