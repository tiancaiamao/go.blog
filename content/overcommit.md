记一例overcommit_memory设置不当导致的线程创建失败

在测试机上发现aerospike起不来了，跟踪原因找到是`pthread_create`创建失败，Resource不足。gdb定位了一下，只能开到第34个线程就挂了。我自己写了个小测试，开到100多个线程的之后会失败。但是怎么会失败呢？ulimit设置的各种相关的都是unlimited的。

然后冷静下来分析，前几天还可以运行的，肯定是哪里改过什么。然后记起来了，改过`overcommit_memory`。看一下它的说明：

	overcommit_memory:
	
	This value contains a flag that enables memory overcommitment.
	
	When this flag is 0, the kernel attempts to estimate the amount
	of free memory left when userspace requests more memory.
	
	When this flag is 1, the kernel pretends there is always enough
	memory until it actually runs out.
	
	When this flag is 2, the kernel uses a "never overcommit"
	policy that attempts to prevent any overcommit of memory.
	Note that user_reserve_kbytes affects this policy.
	
	This feature can be very useful because there are a lot of
	programs that malloc() huge amounts of memory "just-in-case"
	and don't use much of it.
	
	The default value is 0.
	
	See Documentation/vm/overcommit-accounting and
	security/commoncap.c::cap_vm_enough_memory() for more information.

解释一下，系统分配给应用的内存是虚拟内存，当应用访问时会发生缺页中断，然后会分配实际内存。而如果有实际内存不足，还有可能使用到swap的换入换出。在 /proc/sys/vm/overcommit_memory 可以看到这个值。sysctl可以设置它。

`overcommit_memory`是linux的虚拟内存相关的一个参数，默认是0，也就是经常观察到的是linux内存分配，malloc分配很大内存也不会失败，但是后面访问分配的内存的时候，可能由于物理内存不足收到SIGNAL使程序退出，也就是"OOM"。

这是一块系统设计上的折中的选择：如果malloc了就分配实际资源，有些程序其实分配很多资源却没使用；而如果不实际分配资源，就是malloc成功了，使用这块内存却还可能失败。于是`overcommit_memory`参数就是用于控制这个行为的。默认0是启发式的，1是总是允许，这个参数被我改成了2，不允许当申请的资源超限额。

	vm.overcommit_memory = 2
	vm.overcommit_ratio = 80

比如这个设置的意思是，不允许分配超过swap+80%的物理内存。

线程创建失败就是由于这个设置导致的啦，用的一台4G的笔记本，内存并不多，线程开多了，就失败了。