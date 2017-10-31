最近被由context引发的一个bug坑得不轻，所以反思一下Go的context的问题。

## context是隐式的约束，没有检测

如果我们写一个函数，比如:

    func f(a int, b []byte) {
    }
    
我们知道它需要哪些参数，编译器是会帮我做检查的，当我调用

    f(3, "sdfsdf")
    
它就会报错。

可是如果是context，就变成了一种隐式的约束，编译器不会帮我们做检查，比如：

    func f(ctx context.Context) {
        a := ctx.Value("a").(int)
        b := ctx.Value("b").([]byte)
    }

Value函数并没有任何保证，编译器不会检查传进来的参数是否是合理。然而f在什么样的上下文里面被调用是不确定的，因此检测被移到了运行时来做。

现在的函数f有一个隐式的约束，它需要从context里面传a和b两个参数，这些信息，在函数f的签名里面都没法体现。
如果我看一个函数，看它的签名没用，还得去读它的实现，这不是扯淡么！

## context的锁争用

context是一层一层往下传的，如果全局都是使用同一个传递下来的context，会出现一个问题：锁争用。

    select {
        case <-context.Done():
    }
    
大家都在同一个对象上面调用的Done函数，channel操作最终会加锁。<del>这个是在etcd项目里面发现的一个问题，他们改了我们也跟着改了</del>。在起goroutine的时候，一般不要用原来的context了，而是新建一个context，原始的context作为父context。这样不同goroutine就不会抢同一个锁。

一般是用的`context.WitCancel()`这个函数：

    go func() {
        ctx, cancel = context.WithCancel(ctx)
        doSomething(ctx)
        cancel()
    }
    
调用WithCancel的时候，会得到一个新的子context，以及一个cancel函数。子ctx会在父context的Done函数收到信号，或者cancel被调用的情况下收到Done信号。

cancel是需要调用，它使得context释放相应的资源。开头提到的bug，就是这个地方被坑到了：这样写代码之后其实有一个假定的约束，即doSomething操作是一个同步的，当它返回以后，相应的context就已经结束了。

然后，我们的代码在doSomething里面函数调了很深之后(a调b，b调c，c调d)，里面有一个开goroutine异步做的操作，于是就傻逼了。那个异步的操作还没完成，就被cancel掉了。

但是这个问题非常难查，为什么？因为单独看两个地方的代码片断，都没有看出任何问题。上面那段代码写的没问题呀，只要doSomething是一个同步操作就行。而看doSomething的逻辑也没问题，它调了其它函数，其它函数继续调更深的函数，只是到了那里，并没有任何关于禁止异步操作的约束说明。

## 不要将任何context保存为成员变量

context的标准用法就是每次都产生一个，然后一层一层往下传。注意，禁止将context捕获了存储下来。不要将任何context保存为成员变量，不要重用它们。

比如，我要做一个sender对象，它有一个Send方法。那么我不能在new的时候把ctx保存下来，在Send的时候使用:

    func NewSender(ctx context.Context) *sender {
        return &sender {
            ctx: ctx,
        }
    }
    func(s *sender) Send() {
        grpc.XXX(s.ctx)
    }

如果调用某个库它需要传一个context，你应该给它当时的上下文，如果没有，可以传`context.Background()`，但是不要像上面那样，创建对象的时候把context保存下来，到对象的方法调用的时候使用。

正确的使用姿势不应该看到context被保存到任何成员变量里面。

## context本质上是动态作用域

上面说到不要将context保存。让我们看一看问题的本质：

    obj = new Object(ctx)
    obj.method(ctx)
  
请问这是同一个上下文么？ No! 一个时创建时的上下文，一个是运行时的上下文。其实正确来写，它们是这样子的：
 
    obj = new Object(ctx1)
    obj.method(ctx2)

那么把ctx1保存下来，给到ctx2用，当然不对。

被坑几次之后会觉得context很难用。我想了一下，其实这个问题跟动态作用域很类似。现代主流编程语言里面，没有任何一个采用动态作用域的，而人们大多习惯了词法作用域，所以思维上很难接受。

正好说一下动态作用域：

    func f() {
        a := 3
        func g() int {
            return a
        }
    }
    
采用词法作用域的语言，无论在哪里调用`g()`，返回的结果都是3。而采用动态作用域的语言，行为完全无法推断：

    a := 7
    g() // 这里返回的是7，a的值是看运行时绑定的，而不是声明时
    a := 3
    g() // 这里返回的是3
    
当你看到函数需要的参数是一个context，可以context是在每次运行时都不同了，仅仅看声明并没有什么信息，是不是很像动态作用域？
