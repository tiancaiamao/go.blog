switch 在 Go 的编译器里面几乎没做过什么优化，性能还是挺低下的。

写成这样子：

	switch c {
		case 0:
		case 1:
		case 2:
		case 3:
		...
	}

跟直接写成

	if c == 1 {
	} else if c == 2 {
	} else if c == 3 {
	}

就没啥太大区别。

也不能责怪编译器蠢，毕竟 Go 语言里面还要支持像 string 类型，支持表达式，以及像这种

	switch c.(type)

C 语言里面 switch 语句是会优化成跳转表的，比较高效。

可以考虑换种写法，

	var jumpTable []func(){}	// 把各个case要做的事情做成函数放到一张表里
	f := jumpTable[c]		// 直接通过case id找到对应的函数
	f()				// 并跳转去执行

我测试了一下 Go 的 switch 到底有多么低效，case 稍多，放了32个。

https://gist.github.com/tiancaiamao/28fd53ff9b29eaac30f84b4ba04e2e7e

对比的结果是：

	switch cost:		70.476µs
	jumt table cost:	9.065µs

大部分时候没有写那么多switch case的需求，但是有一个场景还是会用到，就是 interpreter。在 C 里面可以做 threaded code，在 Go 里面做不了。我还是希望做出 Go 能达到的最佳优化，于是把 shen-go 的指令分发从 [switch方式](https://github.com/tiancaiamao/shen-go/commit/92ae53e4555a09a215e9546ade466d682195906e#diff-89570c464637c8d662a375fdd8f47b90L145)切换到了 [jump table](https://github.com/tiancaiamao/shen-go/commit/92ae53e4555a09a215e9546ade466d682195906e#diff-89570c464637c8d662a375fdd8f47b90R149)。

结果很今人失望，性能反而下降了。分析原因，主要是 swtich 的写法可以充分利用到函数内联，比如

	switch c {
		case opPrimCall:
			primCall()
		case opPop:
			stackPop()
	}

这里面的primCall和stackPop这些都是可以inline的，而jump table由于函数地址是动态获取的，利用不到编译器的inline优化。

	vm.code[vm.pc](vm)

相当于节省了switch定位具体哪一条操作的开销，却引入了额外的函数调用开销。

有没有什么方法能够既不使用低效的 switch，又能充分利用到函数内联呢？
