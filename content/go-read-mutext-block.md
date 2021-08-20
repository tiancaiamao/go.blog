发现一个神奇的问题：

```
package main

import (
	"fmt"
	"time"
	"sync"
)

func main() {
	var mu sync.RWMutex

	go func() {
		mu.RLock()
		time.Sleep(10*time.Second)
		mu.RUnlock()
	}()

	time.Sleep(100*time.Millisecond)

	// go func() {
	// 	mu.Lock()
	// 	mu.Unlock()
	// }()

	time.Sleep(100*time.Millisecond)

	start := time.Now()
	fmt.Println("before get read block")
	mu.RLock()
	fmt.Println("inside read load no block", time.Since(start))
	mu.RUnlock()
}
```

这一段代码，如果去掉注释的那一段...最后面的读锁会阻塞住！

也就是说，只存在读锁的时候，读读不阻塞。
但是当存在写锁的时候，**读锁能阻塞读锁**...这跟我之前的认知不太一样，用了这么久 Go 我居然都不知道自己的理解行为一直有问题。


查了一下[官方的标准库文档](https://pkg.go.dev/sync#RWMutex)是这么说的：

```
If a goroutine holds a RWMutex for reading and another goroutine might call Lock, no goroutine should expect to be able to acquire a read lock until the initial read lock is released.
```


这也就是上面代码看到的现象了：一个 goroutine 持有 RWMutex 的读锁，另一个 goroutine 调用写锁的时候，没有其它 goroutine 可以在最初的读锁释放之前，再获取到读锁。

－－－－－－－－－－－－－－－－－

说说我怎么遇到这个问题的。弄了一个 map 容器，里面有放了地址到连接的映射。如果连接闲置了，就去回收掉。
用了一个读写锁，有 goroutine 在拿 map 里面的连接出去使用时，加上读锁。
回收不活跃连接时，加上写锁。
持有读锁之后，去发请求的操作有可能会卡住，但预期是不影响其它的连接继续的使用，或者说只是这一个地址的连接卡住。然而...
duang! 当回收连接那个锁开始调用，哪怕还没有持有到写锁，读读也相互 block 了！

好啦，以后使用读写锁的代码，需要更加注意，读读也是可能阻塞的。
