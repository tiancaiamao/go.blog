前段时间开始goagent不好用了。不知道GFW又升级什么高科技，大概是开始封gae了。于是去试了下snova，感觉不怎么好用。做的好的方面是支持https，不好的地方是gsnova客户端的一些细节处理得不是很好。客户端异常退出有些情况下会把服务端搞挂，然后就是加载一些复杂的页面，比如乱七八糟的广告和50个张图以上的页面，会永久卡死，我估计是channel相关的没处理好导致阻塞了。

看了一下snova的代码，其实我需求没snova搞的那么复杂，只用到其中的c4的proxy，也没必要把PAC做到里面，因为在SwitchSharp中已经做过了。

于是干脆自己写一个算了，反正程序员喜欢重造轮子。我觉得，必须要简单，这个东西代码不应该超过1000行，配置也不需要，我自己能用就行了。基本的设计还是跟goagent/snova差不多的，本地运行一个后台程序，连到国外免费云服务器上。

就把本地和远端的分别称作client和server吧。client与server维持一条websocket长连接。为什么不是由浏览器直接连远端呢？这是因为很多免费云都有一些限制的，从外面发起到里面的，只有少数端口和协议可用。直接连是连不了的，所以绕一下。

websocket是基于http的，肯定没哪个云会禁用http，就由它来维护一个长连接的双向通信。browser发请求到client，client处理后通过websocket发到server，server发起实际的http请求，将响应写到websocket。然后client这边再读websockt后分包，发回browser。中间就是一些合包分包的事情，每次请求为一个session，通过session的id进行合包和分包。

-------

8.13更新：

直接拼凑别人的轮子实现了，放在[github](https://github.com/tiancaiamao/n6bagent)，代码量精简到50行，所以直接粘贴上来，服务端的：

	import (
	    "code.google.com/p/go.net/websocket"
	    "github.com/elazarl/goproxy"
	    "github.com/hashicorp/yamux"
	)
	
	func websocketCallback(ws *websocket.Conn) {
	    session, err := yamux.Server(ws, nil)
	    if err != nil {
	        log.Println("websocket serve error:", err)
	        ws.Close()
	        return
	    }
		
	    proxy := goproxy.NewProxyHttpServer()
	    proxy.Verbose = true
	    err = http.Serve(session, proxy)
	    if err != nil {
	        log.Println("Serve error:", err)
	        ws.Close()
	        return
	    }
	}
	
	func main() {
	    http.HandleFunc("/", websocket.Handler(websocketCallback))
	    http.ListenAndServe(":8080", nil)
	}

客户端的：

	import "github.com/hashicorp/yamux"
	
	func NewProxy(hostAddr string) (*yamux.Session, error) {
	    origin := "http://" + hostAddr + "/"
	    url := "ws://" + hostAddr + "/websocket"
		
	    ws, err := websocket.Dial(url, "", origin)
	    if err != nil {
	        return nil, err
	    }
		
	    return yamux.Client(ws, nil)
	}
	
	func Handle(c *yamux.Session, lconn net.Conn) {
	    rconn, err := c.Open()
	    if err != nil {
	        log.Println("multiplex打开连接失败...")
	        lconn.Close()
	        return
	    }
		
	    go io.Copy(rconn, lconn)
	    io.Copy(lconn, rconn)
	}
	
	func main() {
	    proxy, err := NewProxy("localhost:8080")
	    listener, err := net.Listen("tcp", ":48101")
	    for {
	        if conn, err := listener.Accept(); err == nil {
	            go Handle(proxy, conn)
	        }
	    }
	}
