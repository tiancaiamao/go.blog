## http中间件

Go语言的http的Handler很好写，下面是一个helloworld例子：

	func helloworld(w http.ResponseWriter, r *http.Request) {
		io.Write(w, "hello world!")
	}

http中间件是指在原来http.Handler的基础上，包装一层，得到一个新的Handler。

	func GETMethodFilter(h http.Handler) http.Handler {
		return http.HandlerFunc(w http.ResponseWriter, r *http.Request) {
			if r.Method != "GET" {
				http.NotFound(w, r)
				return
			}
			h.ServeHTTP(w, r)
		}
	}

任何一个函数，如果它可以接受一个http.Handler，并返回一个新的Handler，这个函数就可以算是一个中间件。GETMethodFilter就是一个http中间件，我们可以将它包装到helloworld上面，得到新的Handler是一个只处理GET方法的helloworld。

	GETMethodFilter(helloworld)

http中间件最大的好处是无侵入性，只要愿意，可以在外面嵌套许许多多层中间件，达到不同的目标。比较可以加上登陆，加上参数合法性校验，加上访问权限过滤，等等等。

## 串联尝试

上面写过一个GETMethodFilter的例子，那么如果我们要过滤掉的是POST方法呢？重复代码是不好的事情。我们可以写一个MethodFilter，接受参数GET或者POST来决定是返回GETMethodFilter或者POSTMethodFilter。

	func MethodFilter(method string, h http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			if r.Method != method {
				http.NotFound(w, r)
				return
			}
			h.ServeHTTP(w, r)
		})
	}

这种写法有缺陷，多了一个参数，它就不是我们之前的中间件形式了，这种不一致这会导致后面写串联函数不方便。我们将只能这样写：

	ParamFilter("some argument", MethodFilter("GET", helloworld))

而无法写成：

	New(helloworld).Then(ParamFilter).Then(MethodFilter)

因为不同的方法需要不同的参数。

## 串联

Go语言中推崇组合。为了真正实现串联，必须要再往上抽象一层。任何东西只要实现了MiddleWare接口，它就是一个MiddleWare。

	type MiddleWare interface {
		Chain(http.Handler) http.Handler
	}

MiddleWare和MiddlewareFunc的关系，很类似标准库中Handler跟HandlerFunc的关系。

	type MiddlewareFunc func(http.Handler) http.Handler

将MethodFilter修改成为一个真正的MiddleWare：

	func MethodFilter(method string) MiddleWare {
		return MiddlewareFunc(func(base http.Handler) http.Handler {
			return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
				if r.Method != method {
					http.NotFound(w, r)
				}
				base.ServeHTTP(w, r)
			})
		})
	}

上面函数式的写法，如果换成面向对象的写法，我们可以写成下面形式：

	type MethodFilter struct {
		method string
	}
	func (m *MethodFilter) Chain(http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			if m.method != method {
				// xxx
			}
			base.ServeHTTP(w, r)
		})
	}

对象是穷人的闭包？闭包是穷人的对象？who cares。不过在Go语言中，我直觉上认为后一种写法性能上应该好一点点，说来话长所以不解释。

用MiddleWare做串联就很简单了。比如说：

	type Chain struct {
		middlewares []MiddleWare
		raw         http.Handler
	}
	func New(handler http.Handler, middlewares ...MiddleWare) *Chain {
		return &Chain{
			raw:         handler,
			middlewares: middlewares,
		}
	}
	func (mc *Chain) Then(m MiddleWare) *Chain {
		mc.middlewares = append(mc.middlewares, m)
		return mc
	}
	func (mc *MiddleWareChain) ServeHTTP(w http.ResponseWriter, r *http.Request) {
		final := mc.raw
		for _, v := range mc.middlewares {
			final = v.Chain(final)
		}
		final.ServeHTTP(w, r)
	}

然后这样子用

	New(helloworld).Then(ParamFilter).Then(MethodFilter)

## 上下文

如果使用http中间件，必然会遇到的一个问题是上下文中传递数据的问题。上下文是指什么呢？每个中间件会从前面的中间件中获取数据，并且可能会生成新的数据传后面的中间件，这些数据传递形成的就是上下文。比如我有一个ParamFilter，这个中间件的作用是验证输出参数是否合法的。那么验证完之后，应该把数据放到上下文中，传给后面中间件去使用。再比如说，如果我写了一个Login的中间件，那么这个中间件可以把session一类的信息就可以放到上下文，后面的中间件就可以从上下文中获取到session。

[gorilla](https://github.com/gorilla/context)的做法是把上下文信息放到了一个全局map中，使用http.Request作为key就可以将上下文获取出来。这个方案优点是对标准库无侵入性。但是也有些缺点，一个是请求结束后还需要清除掉map[r]，另一个是全局map的加锁会影响性能，这个缺点在很多场景是致命的。

Go语言[官方推荐的做法](http://blog.golang.org/context)是，在每个Handler都加多一个参数context。比如写一个

	type ContextHandler interface {
		ServeHTTP(ctx context.Context, w http.ResponseWriter, r *http.Request)
	}

官方的补充库中专门有一个[context.Context接口](https://github.com/golang/net/tree/master/context)。这种方式对标准库的Handler侵入性比较强，如果有较多遗留代码，也不算太好一个方案。

使用http中间件，处理上下文是必不可少的。上面都是比较有代表性的方案，但是我都不太满意。直到突然有一天灵光一闪，发现其实可以利用http.ResponseWriter是interface这点，把上下文隐藏在里面!

	type ContextResponseWriter interface {
		http.ResponseWriter
		Value(interface{}) interface{}
	}

这样我们可以写标准的http.Handler接口，并且在需要的时候又可以取出上下文：

	func HelloWorld(w http.ResponseWriter, r *http.Request) {
	    if cw, ok := w.(ContextResponseWriter); ok {
	        value := cw.Value("key")
	        ...
	    }
	}

## 完整例子

	package main
	
	import (
	    "github.com/tiancaiamao/middleware"
	    "io"
	    "net/http"
	)
	
	type MyContextResponseWriter struct {
	    http.ResponseWriter
	    key, value interface{}
	}
	
	func (w *MyContextResponseWriter) Value(key interface{}) interface{} {
	    if w.key == key {
	        return w.value
	    }
	    return nil
	}
	
	func HelloWorld(w http.ResponseWriter, r *http.Request) {
	    if ctx, ok := w.(middleware.ContextResponseWriter); ok {
	        valueFromContext := ctx.Value("xxx")
	        io.WriteString(w, "hello, "+valueFromContext.(string))
	        return
	    }
		
	    io.WriteString(w, "hello world")
	}
	
	type MiddleWareDemo struct{}
	
	func (demo MiddleWareDemo) Chain(h http.Handler) http.Handler {
	    return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
	        cw := &MyContextResponseWriter{
	            ResponseWriter: w,
	            key:            "xxx",
	            value:          "demo",
	        }
			
	        h.ServeHTTP(cw, r)
	    })
	}
	
	func main() {
	    handler := middleware.New(http.HandlerFunc(HelloWorld), MiddleWareDemo{})
	    http.ListenAndServe(":8080", handler)
	}

代码放到了[github](https://github.com/tiancaiamao/middleware)，需要的自取。