今天写一个地方代码的时候觉得，要是 Go 语言支持"协变"就好了。不过不是搞 programming language 的人可能不知道协变是个啥 feature。所以就乘着这个机会，介绍一下好了。

协变与逆变，wikipedia 的条目在[这里](https://zh.wikipedia.org/wiki/%E5%8D%8F%E5%8F%98%E4%B8%8E%E9%80%86%E5%8F%98)。不过大伙也不爱去看。

还是需要简单解释一下，先从基类和派生类开始。

Animal 是基类，然后 Cat 是派生类。一个 Cat 是一个 Animal。如果一个函数，它的参数接受一个 Animal，那么这个函数应该是也接受一个 Cat 的，正常的面向对象语言都支持这种特性。

一群 Cat 自然也应该是一群 Animal。同样，由于 Dog 也是 Animal，那么一群 Dog 自然也是一群 Animal。还有，一群 Dog Cat 混合起来，那也是一群 Animal。

所以呢，如果一个函数参数，它的参数是一群 Animal，这个函数是否支持传一群 Cat，或者是传一群 Dog，甚至是传一群 Dog Cat 的混合体呢？

这个就是所谓的协变。如是答案是支持，那这门语言就是支持协变的。

```golang
type Animal interface {
    animal()
}

type Dog struct {}
func (d *Dog) animal() {}

type Cat struct {}
func (c *Cat) animal() {}

var dog *Dog
var cat *Cat
var dogs []*Dog
var cats []*Cat

func f(a Animal) { ... }
f(dog)  // 可以的，dog 是 Animal
f(cat)  // 可以的，cat 是 Animal

func g(a []Animal) { ... }
g(dogs) // 不支持 dogs as animals
g(cats) // 不支持 cats as animals
```

Go 语言是不支持协变的。

* 协变：一个 []Cat 也是一个 []Animal
* 逆变：一个 []Animal也是一个 []Cat
* 不变：以上二者均不是

逆变看起来比较反直觉，不过在函数的子类型里面有逆变的概念。

比如我们有这样一个函数，它的输入是 Dog，返回值是 Animal。

```
func g(d Dog) Animal {}
```


然后我们有这样一个函数：

```
func use(fn func(Dog) Animal)
```

看 `use` 这个函数签名，它接受的参数是一个函数，函数的输入是 Dog，返回值是 Animal。我们都知道，`use(g)` 是可以的(废话！)。


那假如我们有另一个函数，它的输入是一个 Animal，返回值是一个 Cat：

```
func f(a Animal) Cat {}
```

重点来了！ `use(f)` 可不可以呢？ 其实是应该可以的。关键点在于，只要用一个 `f`，肯定可以构造出来一个 `g`。

```
func g(dog Dog) Animal {
    cat = f(dog as Animal)
    return cat as Animal
}
```

参数转换，dog 先转成 animal 作为 f 的参数，然后返回值 cat，再转成 animal 了返回。也就是说通过参数转换，f 是可以实现 g 的。

f is a g 就好比 Cat is a Animal。我们说，Cat 是 Animal 的子类，那么 f 也就是 g 的子类。

看函数签名，f 签名是 Animal->Cat，而 g 是 Dog->Animal。所以 Animal->Cat 是 Dog->Animal 的子类。

再观察一下这个函数子类型：对于返回值，Cat is Animal 是协变，而对于参数，Animal is Dog 这就是逆变了。

说完了协变逆变的概念，回到 Go 语言不支持协变的话题上来。最初我说的，写代码会不爽。

举个例子，我要确认是否所有动物都是健康的：

```
func allHealthy(arr []Animal) bool {
    for _, a := range arr {
        if a.sick() {
            return false
        }
    }
    return true
}
```

要是有协变我们就可以这么玩：

```
var dogs []Dogs
allHealthy(dogs)
```

但是没有协变，代码要这么写：


```
var dogs []Dogs
animals := make([]Animals, 0, len(dogs))
for _, dog := range dogs {
    animals = append(animals, dog)
}_
allHealthy(animals)
```

注意，这里转换一遍是有内存分配的！让人很不爽！

换个思路，我们可以这么写：

```golang
type animalSlice interface {
    Len() int
    At(i) Animal
}
func allHealthy(arr animalSlice) bool {
    for i:=0 i<arr.Len() i++ {
        animal := arr.At(i)
        if animal.sick() {
            return false
        }
    }
    return true
}
```

```
// 让一群 Dog 实现一群 Animal 接口
// 类似地，一群 Cat 也实现 animalSlice 接口
type dogSlice []Dogs
func (dogs dogSlice) Len() {
    return len(dogs)
}
func (dogs dogSlice) At(i int) Animal {
    return dogs[i]
}
// 使用的时候
allHealthy(dogSlice(dogs))
allHealthy(catSlice(cats))
```
