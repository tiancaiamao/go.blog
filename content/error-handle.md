在出错的情况下，如何处理好资源的释放，在Go语言里面相当考验使用者经验。如果能把错误处理做到位，基本上可以算玩得很溜了。资源主要是指goroutine，但这个话题并不特指。记得以前就说过，对于并发编程，我一直是心存敬畏之心的，即使有Go这么强大的语言。

这里先提出三个问题，并没有给出标准的答案。可以一起探讨。

1. error的时候要不要调close
2. close的时候又error了怎么办
3. cancel的时候是否返回error

--------------------------------------------------------------------

好，现在先放下这些问题不谈，来看一段代码。

    func worker(done chan error) {
        err := doSomething()
        if err != nil {
            done <- err
        }
        done <- nil
    }
    func f() error {
        doneCh := make(chan error)
        for i:=0; i<workerNum; i++ {
            go worker(doneCh)
        }
        for i:=0; i<workerNum; i++ {
            err := <-doneCh
            if err != nil {
                return err
            }
        }
        return nil
    }

这段代码的错误处理有什么问题？先不要看答案，思考几分钟...

...

......

假设 `<-doneCh` 返回error了，那么函数f直接返回，之后再也没人读doneCh里面的数据，于是worker的goroutine会永远阻塞在 `done <- err` ---- 泄漏了！

这是非常一个典型的例子，worker的goroutine无法释放，那么它引用的内存就无法回收，程序就会内存泄漏。再回头看一个这篇文章的标题：错误处理与资源释放。在错误处理做的不到位的时候，很容易漏掉资源的释放。

这个地方就是遇到error直接退出导致的。有些简单的修改的办法，比如：

方案一，带buffer的channel，直接让channel大小跟worker数量一样大，就不会有`done <- err`的阻塞问题，总是放得进去的。

        doneCh := make(chan error, workerNum)

方案二，调用者即使遇到了错误，也等所有worker完成之后才返回。

        var err error
        for i:=0; i<workerNum; i++ {
            err1 := <-doneCh
            if err1 != nil && err == nil {
                err = err1
            }
        }
        if err {
            doSomething()
        }

这样写代码又容易遇到另一个问题，即error覆盖，保留第一个，还是保留最后一个？其它打log？到底怎么做比较好呢？可以想一想。

方案三，单独加一个closeCh，专门用于退出。

    func worker(done chan error, closed chan struct{}) {
        err := doSomething()
        select {
        case done <- err:
        case <-closed:
        }
    }
    func f() error {
        doneCh := make(chan error)
        closed := make(chan struct{})
        for i:=0; i<workerNum; i++ {
            go worker(doneCh, closed)
        }
        for i:=0; i<workerNum; i++ {
            err := <-doneCh
            if err != nil {
                close(closed)
                return err
            }
        }
        return nil
    }

close一个channel时，所有读这个channel的goroutine都可以收到通知，[我以前说过这个技巧](go-leak.md)。

-------------------------------------------------------------

代码看完了，思维再跳出来，把问题抽象一下。

我们有可能遇到这样的场景，有A/B/C一个序列的操作要完成，而每个操作都会占资源。如果我们处理到A的时候已经遇到错误了，是否应该回收B和C的资源？答案是肯定的啦，正确的错误处理，不应该发生资源泄漏。假如资源提供了一个叫Close()的操作，遇到错误的场景，我们是应该掉用Close()的。什么时候调用呢？

采用不同的约定方式，有两种处理close的时机：

第一种是，在发生error的位置，妥善处理好资源的释放。即处理完A遇到错误，不能直接return，要先close掉B和C的资源。上在方案二和方案三都是这样的例子。

另一种，把error处理跟资源释放独立起来。流程是这样的，先分配好ABC的资源，函数`f()`里面依次调用ABC，不管`f()`是否返回error，都是f返回后在外面释放资源。可以把方案三改一下作为例子：

        // 统一分配好资源
        closed := make(chan struct{})
        doneCh := make(chan error)
        for i:=0; i<workerNum; i++ {
            go worker(doneCh, closed)
        }
        // 在f里面处理事件ABC
        err := f()
        // 再统一释放资源
        close(closed)

close的时候又error了怎么办？这个问题太恶心了。发生error了想处理好资源释放再返回，然而释放资源也可能继续error，非常需要注意到error的覆盖。

方案一和方案二的代码其实有那么一点不完美。为什么呢？如果遇到错误，调用者对剩下的结果已经不关注了。对比一下，方案三里面，如果遇到第一个出错了，就会调close，直接取消掉后面的运算。而方案一和方案二，还会把运算做完，浪费资源。方案三某种意义上讲，其实就是支持了cancel操作。

Go的标准库里面有个context包，是可以提供cancel的。这是一个非常牛逼的feature。但是有个问题，cancel操作跟普通的错误处理的调用链是完全不同的。普通的错误处理的调用链是一级一级的，像一个链表，而cancel像一颗树。这个思考起来又是一个很头痛的问题。
