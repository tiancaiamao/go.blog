[上篇文章](./go-http-middleware.md)中提到了一下这个问题，但我之前只是写“直觉上认为”，并且还不解释。因为我了解Go的底层，知道方法和闭包分别是怎么实现的，然后用直觉做的判断。但是事后觉得写博客不能这么不严谨，所以做了个测试。

结论大致是对的，但结果也有一些出乎之间意料的地方。

假设我们要实现下面这个接口：

	type I interface {
		XXX()
	}

分别用闭包和对象的方式实现。闭包或是对象，在这里都是想封装一些状态。用对象的写法：

	type Object struct {
		A int
	}
	
	func (obj *Object) XXX() {
		obj.A++
	}

用闭包的写法：

	func Closure() func() {
		var A int
		return func() {
			A++
		}
	}

为了实现接口I，闭包写法需要一点辅助：

	type IFunc func()
	
	func (f IFunc) XXX() {
		f()
	}

现在测试：

	var (
		obj I
		clo I
	)
	
	obj = &Object{}
	clo = IFunc(Closure())
	
	for i := 0; i < 10000000; i++ {
		obj.XXX()
	}
	for i := 0; i < 10000000; i++ {
		clo.XXX()
	}

在我的机器上得到的结果：

	object: 30.026664ms
	closure: 54.920985ms

所以说结论大致是对的。

-----------------------------------------

我又测试了一下直接调用Object.XXX()跟Closure()()，两者性能其实没差异。主要问题是在闭包写法多了一层转换步骤，直觉地认为闭包比方法慢是有偏见的。这是我所说的有点出乎意料的地方。

Go语言中，方法在[底层的实现](https://github.com/tiancaiamao/go-internals)，实际上就是普通函数多加了一个参数，将对象作为这个参数。比如上面的

	func (obj *Object) XXX() {
		obj.A++
	}

变换为底层的处理就是

	func XXX(obj *Object) {
		obj.A++
	}

至于闭包，则是会将一个函数指针和各个upvalue打包到一起。

	func Closure() func() {
		var A int
		return func() {
			A++
		}
	}

转化之后等价于

	func fun001(A *int) {
		(*A)++
	}
	func Closure() struct {
		return &struct{
			fun001,
			new(int),
		}
	}

应该说，性能上真的没有差异。可能对象写法比闭包写法对内存更友好一点点。

如果用两种方式都可以实现时，到底是选对象还是闭包呢？如果纠结的话，我还是更推荐对象写法一些。如果不纠结的话，都可以，无所谓。