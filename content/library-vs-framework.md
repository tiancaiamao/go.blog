其实我一直是喜好库而厌恶框架的，库是你调用它，而框架是它调用你。
就好比一种情况是自己在写代码，另一种情况是别人写好了代码，你在其中填填补补，这种感觉是不一样的。
当然，这里说的是体验感，效率是另外一回事。

今天遇到一个绝好的例子，来举例说明库和框架的区别。

有一个函数，它的签名是类似这样子的：

```
func iterTxnMemBuffer(mb kv.MemBuffer, kvRanges []kv.KeyRange, fn processKVFunc) error
```

它会遍历一块 key-range，然后你调用者传入的 `processKVFunc`，去解析 key-value 并处理:

```
type processKVFunc func(key, value []byte) error
```

这就是典型的框架式搞法，它帮你把怎么样遍历 key-range 搞好了，只需要你提供处理 kv 的方法。

我把这个函数重构了一个，改成了 Iterator 形式的接口：

```
type txnMemBufferIter struct { ... }
func (iter *txnMemBufferIter) Next() (key []byte, value []byte, err error) 
type processKVFunc func(key, value []byte) error
```

修改之后，它就从一个回调形式的接口，变成了一个主动调用形式的接口，调用者需要自己去处理遍历过程。

注意，修改之后的接口是"可组合的"，而前面的接口则是把 iterator 和 decoder 两个东西，耦合在一起了。

耦合之后的可重用性会变差，比如说我需要一个批处理逻辑，处理 5 条 key-value 记录，执行一个 batch 操作，然后再取下一批数据处理，根据上一批的处理结果，对下一批可能会不同的操作。

`iterTxnMemBuffer` 接口就很难实现这个功能，传一个很复杂的对象进来，里面维护状态机，回调再回调。
而用下面重构后的形式，则很容易实现。因为 iterator 在内部保存状态了。

这个就很能反映库和框架的区别。框架很多东西写死了，然后使用者去填填补补，on update 做什么，on draw 做什么，驱动逻辑不是在用户自己手上。库则具有这种灵活性。
