真是受不了项目里面，一些接口被搞得越来越庞大，所以来吐糟了。rob pike 教导我们说，接口应该尽量地小：

> The bigger the interface, the weaker the abstraction -- Rob Pike

只有小的接口，才是可组合的。否则去写 java 好了，为啥要来写 Go 呢？

最糟糕的一类是这样子写的：

```
type I interface {
   ...
   XX()
}

type T1 struct {}
func (t T1) XX() {
}

type T2 struct {}
func (t T2) XX() {
    panic("unsupported")
}

```

如果定义了一个接口中，某些实现需要做，而另一些实现又不支持的，说明此方法根本不通用！即使不是通用的方法，为什么要往接口里面塞呢？Don't panic，[反面教材](https://github.com/pingcap/tidb/blob/0dd87e980661ec8cdf699d5bb2ef28852eed2ea0/infoschema/tables.go#L2109)。

其次糟糕的一类：

```
func f(t I) {
    switch t.(type) {
        case T1:
        case T2:
    }
}
```

都定义了接口了，却需要用 type switch 去看底层是哪种实现，这说明这个接口抽象的不好呀！依赖于接口而不是实现。

还一类常见的糟糕的地方，冗余的方法全部往接口里面塞，这里一个[反面教材](https://github.com/pingcap/tidb/blob/55b3e1110ca4cece87283debc2fddd327182ee11/table/table.go#L198)：

```
type Table interface {
        ...
	AllocHandle(ctx sessionctx.Context) (int64, error)
	AllocHandleIDs(ctx sessionctx.Context, n uint64) (int64, int64, error)	
	Allocator(ctx sessionctx.Context, allocatorType autoid.AllocatorType) autoid.Allocator	
	AllAllocators(ctx sessionctx.Context) autoid.Allocators
}
```

Table 接口里面为什么有这么多 `Alloc` 相关的方法？而且，其中一些还是可以用另一些推导出来的。难道直接定义返回一个 allocator，不香么？难道把各个 table 各个实现里面都加几个这样的函数很有意思么？

对冗余的处理，我们可以简单的只保留一个。剩下的去包装一下就好。比如，下面都是等价的写法。

```
func AllocHandle(ctx sessionctx.Context, t Table) (int64, error) {
    allocator := t.Allocator(ctx)
    allocator.Alloc(id)
}

AllocHandle(ctx, t)
```

跟这个：

```
func (t *tableImpl) AllocHandle(ctx sessionctx.Context) (int64, error) {
    ...
}
t.AllocHandle(ctx)
```

区别是前一种不会把冗余的方法丢到接口定义里面，多个不同的 Table 实现，不会出现[代码](https://github.com/pingcap/tidb/pull/15227/files#diff-7a9e970fa682861b6af9689a4a8088daL2108)[冗余](https://github.com/pingcap/tidb/pull/15227/files#diff-7a9e970fa682861b6af9689a4a8088daL2250)：

```
func (t1 T) AllocHandle() { ...}
func (t2 T) AllocHandle() { ...}
func (t3 T) AllocHandle() { ...}
```

有些接口有一个默认实现，算是基类吧，然后派生类都是继承这个基类的，通常使用的结构体嵌入的形式。

```
type base struct {}
type derive struct {
    base
    ...
}
```

随着代码的腐烂，基类里面的方法越来越多了。然后这个接口也就越来越大，越来越恶心了。比如说这里有一个[坏的例子](https://github.com/pingcap/tidb/pull/10846/files#diff-a223963715d7c23da698d5963075810eL174)。

```
type Executor interface {
	Open(context.Context) error
	Next(ctx context.Context, req *chunk.RecordBatch) error
	Close() error
	Schema() *expression.Schema

    // 这里就把基类的一些细节在往接口里面加，接口就被搞大了
	retTypes() []*types.FieldType	
	newFirstChunk() *chunk.Chunk	
}
```

这就涉及到对共同部分的处理。既然基类是所有继承类都是带的，我们可以在接口里面定义一个方法把基类返回出去。这样往基类加东西就不会影响接口的方法：

```
type Executor interface {
	base() *baseExecutor
	Open(context.Context) error
	Next(ctx context.Context, req *chunk.Chunk) error
	Close() error
	Schema() *expression.Schema
}
```

[好的例子](https://github.com/pingcap/tidb/blob/e58cef5718208a0e8858a2510c811d004570bfe3/executor/executor.go#L190)。

再说一个场景，即想调用基类的公共函数，又想派生类中重载，要怎么搞？

```
type base struct{}
func (b base) Method() {}

type derived struct {
    base
}

var d derived
d.Method()  // 这个会调用到 base 的 Method()
```



如果用 `derived` 重写掉 `Method()`，每个派生类都要把这一段代码重写一遍：

```
type derivd1 struct{}
func (d derived1) Method() {
    d.base.Method()
    ...
}

type derived2 struct{}
func (d derived2) Method() {
    d.base.Method()
    ...
}
```

答案是，用函数包装：

```
func Method(b derived) {
    f.Base()
    ...
}
```

多写写函数，把接口对象当参数传进去，效果一样的，而不必在接口添加方法，好好体会一下。

随便你们怎么写代码，总之，求你们别再往接口里面乱加方法了！
