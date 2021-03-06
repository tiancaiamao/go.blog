无意中受libtask启发看到getcontext这函数，一下子学得我应该能自己写一个用户级线程库了。以前做个课程设计时写嵌入式操作系统内核，也写过进程调用的东西。看到getcontext函数后，我又想练练手了。getcontext把最难搞的保存上下文信息弄好了。若是自己写必须用汇编，还必须了解对应硬件体系结构的东西

有几个难点要处理:

1. 栈大小不够时，如何自动增长?
2. 是抢占式的还是协同式的?

采用不可剥夺，协同方式进行线程切换。因为在用户级上采用不可剥夺效率会更高。而且用户应该知道更合理的时机进行切换。提供 `task_yield` 函数

栈增长的方向要注意，应该是向下的。而数组的增长方向是向上的。

## 线程的数据结构组织方面

给一个 tid，如何直接得到对应的 `task*` ? 存一个 `task*` 的数组 all，这样就可以通过 tid 直接得到对应的线程控制结构了。

ready 队列搞一个。利用链表把就绪队列管理起来

freeslot 分配 `task*` 的糟位

../img/task.jpg 

最后整个程序退出时要释放 task，如何编历到所有分配的资源？先把 freeslot 链表的所有都置为 NULL，然后扫描一遍 all，对不为 NULL 的项进行释放。

`task_create` 流程：

    分配结构体的空间
    初始化成员，包括 context
    放到 ready 队列中

schedule 流程：

    死循环
    从ready队列中取一项作为running，如果没有ready的了，准备退出
    进行swapcontext切换到running的上下文
    返回这里意味running做了上下文切换，可能原因是主动yield，读写channel而block，以及线程退出
    如果原因是该task要退出，则释放资源
    把running置为NULL

`task_exit` 流程：

    把 task 的 status 置为EXITING。要 switch 到 schedule 中才能真正释放空间，因为 taskexit 函数使用的是此 task 的栈
    把对应的 slot 回收给 freeslot 进行 swapcontext 换到 schedule 的上下文

`task_yield` 流程：

    把当前running挂到ready队列
    swapcontext到schedule的上下文

线程通信方面。只支持 channel

../img/task1.jpg

channel 中分别挂着阻塞队列(数组)，因无法写管道而阻塞的挂在 sender，因读管道阻塞的挂在 receiver。队列的每个元素就是 struct task*

不单独设置 block 队列
管道可以是带缓存的也可以不是带缓存的。不带的用 `void *p`
带的有个数组实现的环形队列

`chan_send` 流程：

    确定是否能写管道
    若不能写，将当前线程挂到此通道的sender链中，然后 swap 到 `schedule_context` 进行调度 \\ 若可写，则写管道，然后从 receiver 中挑一个放到 ready 队列
    然后函数返回0表示此函数调用没有被阻塞过

## 非阻塞IO

用户级线程，假如其中用个线程IO阻塞了，则整个进程也会被阻塞

如何解决这个问题？让某个线程IO阻塞后调度器自动切到其它线程继续运行？

核心思想：将IO的文件描述符设置为非阻塞，然后专门开一个后台线程做epoll

fd设置为不阻塞 fdnoblock
fdread中，若error==EAGAIN则调用fdwait
fdwait函数把fd加到pollfd中，然后切换线程
开一个fdtask线程专程检测就绪的pollfd，此线程的流程：

    无限循环：
    while(task_yield() > 0);    //确定没有其它task运行时才做IO
    if(poll(pollfd,npollfd,ms) < 0) {  //有可运行的fd
        for 每个可运行的线程
            调用task_ready

代码放在 https://github.com/tiancaiamao/task

2012.8.23 补充： 两个改动

1. 想让栈空间大小不受限制 受云风的coroutine库启发，每次切换前把线程的栈（堆空间）拷到主栈上，切换后把主栈上该栈的空间拷回堆区保存
2. 去掉task_main 让程序还是从main函数起，然后调用task_init初始化。一直运行都是用的主栈的栈空间。

2012.8.30 在主栈后面拼接的拷贝方式无法实现。 因为没有函数直接提供改当前栈的地址，并继续运行某个context
