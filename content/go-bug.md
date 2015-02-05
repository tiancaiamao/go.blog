## 复现

重构以前一个项目的代码，改动比较大，测试的时候出现一个bug。精简到最极致的复现方式是这样子的：

	package main	
	import (
		"fmt"
		"github.com/gorilla/context"
		"github.com/gorilla/mux"
		"net/http"
	)
	
	func main() {
		r := mux.NewRouter()		
		r.HandleFunc("/", helloworld)
		http.ListenAndServe(":8080", r)
	}
	
	func helloworld(w http.ResponseWriter, r *http.Request) {
		fmt.Println("hello world")		
		input := context.Get(r, main)		
		fmt.Println("never run here", input)
	}

从那一坨坨的业务代码中，定位到这个级别的复现，还是颇费了一番工夫的。

上面的程序，表现形式是，请求`http://localhost:8080/`之后整个程序都会挂起，不会打印"never run here"，程序不再接受任何请求。如果写了其它
的router，也不会响应的。

也许很多人能走到这一步，就已经开开心心的给[gorilla](https://github.com/gorilla/)提bug去了。但是，那样就不能算本文标题中的“深入跟踪”了。也许，读者对bug深入研究不感兴趣，那么... TL;DR

## 分析

程序无响应最可能的两种情况，一种是死循环了，另一种是死锁了。查看CPU使用率很低，不像是死循环的样子。然后去读gorilla库的代码，有看到使用mutex，初步推测可能有死锁。

程序是运行到了context.Get后出问题的，这里是context.Get的源代码，乍看之下并没有发现什么问题：

	func Get(r *http.Request, key interface{}) interface{} {
		mutex.RLock()		
		if ctx := data[r]; ctx != nil {
			value := ctx[key]
			mutex.RUnlock()			
			return value
		}
		mutex.RUnlock()
		return nil
	}

我加了几行print之后，发现是在ctx[key]这里没有再返回。

然后开始猜测，这一行并不是channel操作，理论上是不应该发生不再返回的情况的。难道map操作引起goroutine切换，然后什么场景下能触发到死锁让这个goroutine再也得不到调度？

现在要判定是gorilla的问题还是Go本身的问题。

我把mux换成标准库的mux后，好像一切正常：

	func main() {
		http.HandleFunc("/", helloworld)
		http.ListenAndServe(":8080", nil)
	}

分析到这里卡了好久，直到我觉得，既然应该是跟mutex相关的，那我去掉试试。我把gorilla库中的mutex那几行代码注释掉了，然后真相大白：发现ctx[key]不返回的原因是这个goroutine挂掉了，不是死锁。

	2015/02/05 12:21:10 http: panic serving [::1]:64076: runtime error: hash of unhashable type func(http.ResponseWriter, *http.Request)
	goroutine 7 [running]:
	net/http.func·011()
		/usr/local/Cellar/go/1.4.1/libexec/src/net/http/server.go:1130 +0xbb
	github.com/gorilla/context.Get(0xc208032a90, 0x246fe0, 0x368c30, 0x0, 0x0)
		/Users/genius/project/src/github.com/gorilla/context/context.go:39 +0x3b8
	main.helloworld(0x713068, 0xc2080445a0, 0xc208032a90)
		/Users/genius/project/test/panic.go:22 +0x11b
	net/http.HandlerFunc.ServeHTTP(0x368c30, 0x713068, 0xc2080445a0, 0xc208032a90)
		/usr/local/Cellar/go/1.4.1/libexec/src/net/http/server.go:1265 +0x41
	github.com/gorilla/mux.(*Router).ServeHTTP(0xc2080400a0, 0x713068, 0xc2080445a0, 0xc208032a90)
		/Users/genius/project/src/github.com/gorilla/mux/mux.go:98 +0x2b9
	net/http.serverHandler.ServeHTTP(0xc20805c120, 0x713068, 0xc2080445a0, 0xc208032a90)
		/usr/local/Cellar/go/1.4.1/libexec/src/net/http/server.go:1703 +0x19a
	net/http.(*conn).serve(0xc208044500)

但是为什么没有打印panic信息？好坑爹!

初步结论：gorilla库中有用到一个全局mutex，如果某个goroutine获取锁后panic掉，没能释放锁，会导致程序不再处理任何请求。(而且由于这个锁的原因，不会打印panic ???)

也许很多人觉得能找到这里，就已经很开心了，可以停下来。但是，为什么会panic呢？不继续找出原因，就不能算本文标题中的“深入跟踪”。

## 谁的错？

其实到上面为止，我还是没找出到底是谁的错，gorilla或是Go自身？who's to blame。问题的关键在于为什么panic。

从panic信息来分析，`runtime error: hash of unhashable type`。gorilla的context底层是用的一个map[interface{}]interface{}的结构，然后我传入一个helloworld函数指针作为key，怎么就panic了？

首先，我想要更简单的重现这个panic。于是试了一下:

	m := make(map[interface{}]interface{})
	v := m[main]

确实是会panic的。难道函数不能作为map的key吗？看看[Go的语言规范](http://golang.org/ref/spec)怎么说。

	The comparison operators == and != must be fully defined for operands of the key type; thus the key type must not be a function, map, or slice. If the key type is an interface type, these comparison operators must be defined for the dynamic key values; failure will cause a run-time panic.

我勒个擦，函数是不能作为Go语言中map的key的...是我的错。

为什么Go语言中函数不能比较操作呢？这个话题跟本文跟踪bug无关，要写也是下一篇了～

----------

另：Go语言规范说用不合规范的interface做key会panic，这个行为目前应该是未定义的。比如在目前Go1.4.1上，这一段代码就不会panic：

	func main() {
		m := make(map[interface{}]interface{})
		m[main] = 5
	}

至于gorilla，不看底层实现并不知道context.Get是不能用函数作为key的。还有崩溃了没有panic打印的问题。gorilla的这种行为是否算是bug，就看各人怎么想了。我没有去提交issue，如果有人认定是bug，去帮忙提一下也无妨。


