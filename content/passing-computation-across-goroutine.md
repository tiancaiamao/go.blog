# goroutine之间移动计算

如何在goroutine之间移动计算？即如何把goroutine G1中的计算"搬"到goroutine G2中去执行？

场景是这样的，有一些goroutine之间共享的数据，只有一个goroutine拥有数据的写权限，其它goroutine只有读权限（若多个goroutine同时对数据有写权限就必须加锁了）。这时某个goroutine G1持有数据D的引用，但却不具备数据的写权限，而另一个goroutine G2具备数据的写权限，希望将对D的计算，从G1"搬"到G2中去执行。

基本思路，是将函数与数据一起，封装在闭包内面，然后通过channel，从一个goroutine中传递到另一个goroutine中去执行。

上代码：

	// shared data
	var data D
	
	func computation(d *D) {
	}
	
	go G1() {
		// d is an reference to shared data D
		d := &data
	
		// a closure send to channel
		ch <- func() {
			computation(d)
		}
	}
	
	go G2() {
		// get the closure from channel
		f := <- ch
		// execute the computation
		f()
	}


