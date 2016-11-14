有界连续，不是要讲数学里面的"有界连续"，而是编程语言里面的"delimited continuation"，姑且将它翻译成"有界连续"，不过后面还是尽量使用英文单词。

continuation表示的是"剩下的计算"。CPS(continutation-passing style)里面的C，就是单词continuation的缩写，CPS的意义在于它提供了一种机制将continuation暴露出来。可能continuation这个概念比较陌生，但其实当里写回调风格的代码时，就已经在接触continuation的概念了。

    rendor(button_click_callback)

假设这是一个模态的窗口，点击以后在`button_click_callback`里面会处理接下来的逻辑。其实`button_click_callback`就是一个continuation，因为它代表的正是“剩下的计算”。

为了暴露控制流而将整个程序做CPS变换，这种方式获得continuation是不怎么友好的。正如回调风格的代码是比较反人类的一样。

有少数语言提供了机制将continuation暴露出来，scheme里面著名的call/cc就是其中一种。区别于delimited continuation，call/cc是一种undelimited continuation。就是说由call/cc得到的连续，是一种完全没什么限制的continuation。可以回到程序执行的任意时刻，可以实现异常、coroutine等等任何东西。

scheme的call/cc虽然方便了用户，但是引出了一些问题，或者我认为它带来的问题比好处多。

首先是性能问题。传统的函数，进入时产生一个栈，返回时这个栈就退出了，将来是没办法再重新回到这个函数的现场的。而在scheme中，既然够将continuation保存下来，将来任意时刻再重回现场，那必然程序的控制结构相对于传统模型会有巨大的变化。其中一种方式是做CPS变换，chicken scheme是这么干的，CPS变换之后的函数永不返回，于是栈只会一直增长不加回收，必须再引入蹦床之类的机制。另一种方式是使用不同的虚拟机模型，比如SECD。这样的模型由于存储环境是基于堆而不是栈，性能都不靠谱。

其次是实现复杂。SECD不靠谱啊，怎么办？想办法优化。比如想办法优化成stack/heap混合存储模型，但优化的代价就是过高的实现复杂度，难以理解，难以实现。

还有个问题是实用性低。call/cc的那种undelimited continuation，虽然理论上说是非常强大，可以实现异常，非局部跳转，生成器，coroutine等等等，但哪个用户又是真正祼的使用call/cc呢？你看lua的coroutine大家都知道吧，其实比起sheme的continuation那是相当的low，但是...resume/yield明显比call/cc好用。

这是关于continuation和undelimited continuation，而接下来才是真正的主题：delimited continuation。

举个例子，一个简单的算术表达式：`3 + 5 * 2 - 1`，如果我们要执行`5 * 2`，那当前的表达式就是`5 * 2`，我们写成`3 + [5 * 2] - 1`来表示。那么continuation(当前连续)就是`3 + [.] - 1`。换句话说，它的意思是"当`[.]`接受一个值之后，它会继续将这个值加上一个3，然后减去一个1"，这就是continuation所谓“剩下的计算”的含义。很类似于一个函数，当它接受一个值之后，继续完成函数体里面的计算。`[.]`可以换成其它东西，比如说抛出异常，那就是`3 + [raise Abort] - 1`。

delimited continuation就是多了一个界，即是给`[.]`会回到哪里定了一个"界"，比如`<3 + [5 * 2]> - 1`，我们用`<>`来表示"界"，那么当前的有界继续`[5 * 2]`是`<3 + [.]>`，而不包含后面的那个减1。

为了方便研究，我们引入shift和reset两个关键字。

`(reset E)`在一个delimited的上下文中执行E，如果在E的执行期间有捕获continuation，那么捕获的连续将以reset为界。`(reset E)`表达式的返回值等于E的返回值。

    (reset (- (+ 3 (* 5 2) 1)))

返回的是12。

`(shift (lambda (k) E))`将当前的有界连续捕获为k，执行表达式E。

`3 + [5 * 2] - 1`，放到reset和shift里面：

    (reset (- (+ 3 (shift (lambda (k) (* 5 2)) 1))))

这个直接返回10，因为我们是丢弃了连续直接返回的。而

    (reset (- (+ 3 (shift (lambda (k) (k (* 5 2))) 1))))

返回的是12。这是正常的执行逻辑，5乘2的结果被返回给 `3 + [.] - 1` 这个continuation，继续执行后得到12。

    (- (reset (+ 3 (shift (lambda (k) (* 5 2))))) 1)

返回的是9。5乘2作为reset表达式的值返回之后，再减去1。

甚至我们可以返回不一样的类型：

    (reset (- (+ 3 (shift (lambda (k) "hello")) 1)))

返回字符串"hello"。

通过shift和reset， `<3 + [5 * 2]> - 1` 映射起来非常直接，简单来说界就是用reset来限制，而连接就用shift来捕获。

异常用delimited continuation描述也是十分方便的：

    try {
      3 + (if 1=1 5 else throw error) - 1
    } catch {
    }

    (reset (- (+ 3 (shift (lambda (k) (if 1=1 (k 5) else error)))) 1))

最后简单提一下delimited continuation的实现。实现方式其实是很多的，比如用undelimited continuation是可以实现出delimited continuation的，那么call/cc就可以实现shift和reset，当然这样子违背初衷了。或者用CPS变换，可以只做局部的CPS。

感觉比较靠谱的一种实现是拷贝栈。在reset的时候在栈上加一个标记，当出现shift的时候，将栈顶到标记之间的内存，保存到堆上，同时连上返回地址之类的上下文信息抽象成一个函数闭包，接着以这个闭包为参数k调用shift的函数。如果之后k被调用了，则将保存的堆现场恢复到栈上，继续完成这个连续，而如果k没有被调用，也就是直接被丢弃了。

由于保存下来的现场只是一段的栈，所以这个continuation是有界的，只能恢复到reset时的位置。
