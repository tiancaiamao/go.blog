枚举类型在ML类语言里面写起来是最自然的，比如ocaml，F#里面：

    type T = 
    | T1 of int 
    | T2 of string
    | T3 of bool

haskell里面好像用的datetype而不是type，不太记得细节。总之这是非常重要的一种类型，并不仅限于函数式语言中。

在C语言里面，没有类型安全的保证。好在C里面有union类型，并且有指针，指针这东西太灵活了，能玩得飞起。在C里面枚举类型可以这样写：

    struct T {
      int kind;
      union {
        char* T1;
        int T2;
        bool T3;
      } value;
    };
    
这里有个小的细节，union分配的大小等于结构体里面最大的那个，所以这种写法不一定最优。C足够灵活，就不多说了。

在Go里面，连union关键字都没有了，类型也做的更严格，指针不像C那样可以随意转。可能比较标准的做法是用interface。interface也是可以玩得飞起的，比如[这篇文章](interface-vs-embedding.md)以前探索过。但是现在回头想想，觉得太复杂。

interface{}像C的指针一样，它可以是任何类型。但是有些问题吧，第一个问题，它的类型安全是靠运行时判断的，相比函数式语言只产生编译期开销，还逊色不少。

另一个问题是对代码洁癖者说的，其实只要代码里出写：

    if _, ok := t.(T); ok {}

我认为基本算得上bad smell了。Go语言讲究依赖于接口而不是实现(其实原则上任何语言都是)，你都使用接口了，还需要反射出具体的实现来，那就是丑。

再一个重要的问题当然是性能了。参数传递会有对象分配，用interface反射出原始值也会有对象分配。即无论你是

    func f(v interface{}) {}
    f(3) // 由于参数是interface，这里会分配一个interface对象，对象的值指向3
   
还是
   
    raw, ok := t.(T)   // raw不是原始值，而是分配一个T并从原始值拷贝。

都是有内存分配开销的。在一些性能重要的场合，这种开销对GC还是危害挺大的。

反正觉得在Go里面用interface实现枚举类型不算是优雅的方案。

---------------------

下面就是我觉得更好的一种写法了。

先定义嵌入类：

    type T struct {
        kind enumT
    }
    
在实现要枚举的T1 T2 T3，都嵌入这个类型：

    type T1 struct {
        T
        v int
    }
    type T2 struct {
        T
        v string
    }
    type T3 struct {
        T
        v bool
    }

实际使用的时候，我们的类型都是`*T`，判断kind去获知真实类型：

    var t *T
	switch t.kind {
	case kindT1:
	case kindT2:
	default:
	}

创建牧举类型的对象的写法：

    var t *T
	tmp := T1{}
    tmp.kind = kindT1
	t = &tmp.T
    
当然最好是封装成函数

    func createT1() *T
    func createT2() *T
    func createT3() *T

可以为kind的类型做一个别名，并定义常量：

    type enumT int
    const (
        kindT1 enumT = iota
        kindT2
        kindT3
    )

用枚举 `*T` 反射出真实的类型 `*T1` 需要用到指针转换，用unsafe，其实也可以用函数封起来：

    var t T
    raw := (*T1)(unsafe.Pointer(t))

相比于用interface，这种写法是跟C的指针一样无开销，并且对于GC也是安全的(用unsafe.Pointer存东西是不安全的)。
