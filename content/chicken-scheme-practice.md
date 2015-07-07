编程语言这东西，还是需要实践才能掌握的。之前尝试过不少的scheme实现，直到看过[chicken](www.call-cc.org)才觉得这就是the one，是一个可以用scheme语言做点实际事情的实现。

一个东西，不管是语言也好，编译器或者运行时库也好，如果里面有用到的的技术我不能理解，我就会认为这东西太过复杂，大概不适合自己。chicken的实现非常简单(我在[这里](scheme-to-c.md)，[这里](control-flow.md)其实都有提及)：把scheme编译成c，然后再编译c代码。编译这边用到一些技术，去语法糖，cps变换，闭包变换等东西，都是不难理解的。运行时是用的拷贝垃圾回收，continuation基于cps的也非常简单。

很久前就折腾scheme语言，但是似乎找不到能实际用它做些什么，所以呢，发现chicken的时候就像寻到宝了，高兴好一阵。看过基本的文档后就试着用它写了一点点练习。

写了一个这样的玩意：一个简单的服务器程序，每收到连接后开启一个沙盒，客户端发来的请求都是S表达式，直接在沙盒环境中执行请求，将结果发回客户端。不一定所有结果都是可序列化的，比如(define a 3)的返回值就是未定义的，直接发送这个结果到客户端会导致客户端误认为收到eof而关闭连接，这种情况我选择返回一个(pong)给客户端。

chicken中的thread([srfi-18](http://wiki.call-cc.org/man/4/Unit%20srfi-18))很轻量，也是纤程，底层是基于continuation的，可以来一个连接开一个thread。[tcp的封装](http://wiki.call-cc.org/man/4/Unit%20tcp)也很好使用，不需要自己去socket/bind/listen什么的。IO也被抽象成了port，发送和接收都是分别可以操作的，非常方便。练习下这些，就算迈入了用scheme做网络编程的大门。

沙盒[sandbox](https://github.com/tiancaiamao/yasfs/tree/master/sandbox)是自己写了一个scheme的解释器，很类似《Lisp in Small Pieces》中fast interpretion那一章的实现，将S表达式编译成宿主环境的lambda。

	(define env (make-sandbox)) ;; 定义一个沙盒
	(sandbox-eval '(define a 3) env) ;; 在这个沙盒中定义a
	(sandbox-eval '(define b 5) env) 
	(sandbox-eval '(+ a b) env) ;; 在这个沙盒中执行代码

可以用make-sandbox生成许许多多的执行环境，沙盒环境完全是隔离开来的，互不影响。

书上只有quote if begin set! lambda几个syntax，其实比较麻烦的是define的实现。因为其它几个syntax的编译器和运行期都比较分明，但define实际上是一个编译时环境和运行时环境处理起来比较模糊的关键字。看下面的代码：

	(define fact
		(lambda (n)
			(if (= n 0)
				1
				(* n (fact (- n 1))))))

在函数内面的fact被编译的时候，编译环境中应该能找到fact，否则编译就要报错了。但这个时候fact的值却是未绑定，应该是到运行的时候才能获取到的。

为什么是写这么个东西拿来实践呢？其实，是mmo游戏编程那边的一些想法。

如果是scheme，甚至可以省掉消息格式约定，直接发送S表达式。比如移动(move X Y)，攻击(attack ID)等。

每个玩家给他一个沙盒，玩家就可以定义自己的插件了。玩家可以定义自己的宏，其实就是scheme代码，发送到服务端。服务端执行以后就保留在沙盒中了，当玩家再调用的时候，服务端会在该玩家的沙盒中执行代码。

把握好沙盒的访问限制，宿主环境只暴露玩家沙盒能访问到的资源。如果用scheme来做mmo，感觉这个想法还是挺好玩的~


