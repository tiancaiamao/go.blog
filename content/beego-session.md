beego的session使用中的一处竞态条件

被session的一个bug坑死了，今天几乎一天时间都耗在定位这个bug了。表现形式是，用户被不正常的踢出，或者登陆进去又被踢出。

代码是类似beego官方教程写的:

	func init() {
	    globalSessions, _ = session.NewManager("memory", `{"cookieName":"gosessionid", "enableSetCookie,omitempty": true, "gclifetime":3600, "maxLifetime": 3600, "secure": false, "sessionIDHashFunc": "sha1", "sessionIDHashKey": "", "cookieLifeTime": 3600, "providerConfig": ""}`)
	    go globalSessions.GC()
	}

	func login(w http.ResponseWriter, r *http.Request) {
	    sess := globalSessions.SessionStart(w, r)
	    defer sess.SessionRelease()
	 	...
	}

使用的文件作为存储。另外，有一个info函数，进入界面之前会先调info，看是否从session中拿到userID，如果拿到了，就说明是登陆了，否则说明是未登陆。info跟login也是类似的:

	func info(w http.ResponseWriter, r *http.Request) {
	    sess := globalSessions.SessionStart(w, r)
	    defer sess.SessionRelease()
	 	...
	}

用户长时间不操作以后，session会过期被清掉，用户再操作的时候会被踢出去。由于后台无法主动通知前端那边session过期了，而session过期后还停留在原的操作界面就很奇怪，所以前端周期性地去调info函数，如果返回信息不对，就302跳到登陆界面。

先说下beego使用文件作为session存储的实现吧。调用SessionStart时会根据请求中的cookie参数拿到session的文件名，读取文件内容(gob编码的)，加载到内存中。每次调session的Get都会更新文件修改时间，后台的GC函数会周期性的遍历目录，清除掉过期的session文件。调用SessionRelease函数会将内存的内容写回到session文件中。

问题就出在这里了：session文件读到内存；修改session中的内容；将修改后的内存写回文件；整个过程并不是原子的。

回放我这里的bug产生的时序：

1. 用户session过期被踢下线了，重新登陆。

2. login和info请求是差不多恰好同时到达的(info是周期性地不停在执行的)。

3. info函数执行，请求中带的session id是A。加载session A的文件失败(session过期消除了)，重新生成一个新的session B

4. login函数执行，请求中带的是session A。加载session A失败，执行登陆流程。登陆成功了，生成session C

5. login返回客户端，告诉客户端cookie记录session C

6. info返回客户端，告诉客户端cookie记录的是session B (总之最后客户端cookie记下的是session B)

7. info和login的defer SessionRelease执行，分别存文件session B和C

8. login告诉客户端登陆成功了，而info却告诉客户端失败了，最后客户端记录的cookie是session B的。

9. 客户端带的cookie后面以session B的身份请求服务端，出错。

------------

为什么会返回不同的cookie给客户端呢？session文件清除之后，调SessionStart发现sessionID对应的文件不存在，都会新建一个session文件，将新的session id发回客户端记录cookie。这个过程不是原子的，login和info同时执行处理session文件不存在，都生成了一个新的session id返回客户端。

客户端拿到的是最后的那个cookie记录的session向服务端发送请求的。请求到的是info写的session，而不是登陆成功写的session文件。

也是第一次用beego的session，没什么经验，踩了坑。

