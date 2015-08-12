[接上篇](chicken-scheme-practice.md)，这次练习，我把自己的技术博客（本网站）用scheme重写了！

## web框架

chicken提供了许许多多的eggs，相当于其它语言中的标准库或者三方库。我使用了[spiffy](http://wiki.call-cc.org/eggref/4/spiffy)这个egg作为这个网站的web框架。

使用spiffy很容易，最简单的例子：

	(use spiffy)
	(root-path "/var/www")
	(server-port 8088)
	(start-server)

root-path是静态的网站文件，可以打开本地的8080访问。

这个博客文件是用markdown写的，还有rss输出之类的东西，并不是完全静态的，所以还得做一些工作，首先是路由。

### router

spiffy的路由是用一个动态变量vhost-map来决定的。有一张表，前面的参数是正则式，后面的参数是具体的handler函数。我把整个路由都自己接管了，所以直接匹配.*的正则。

	(vhost-map `((".*" . ,router)))

接下来我可以在router中做自己的派发。规则很简单，主要的博客文章都是用markdown写的，文件名是以.md结尾。有一些旧版的文件是用emacs的org-mode生成的html。还有一些以前用Go的present格式写的文件我已经手动改成markdown格式了。为不同的文件格式做不同的处理，各自有自己的handler。

### handler

handler是一个类似如下形式的函数

	(define router
		(lambda (continue)
		...))

注意到参数中除了continue什么都没有，如何拿到request，如果返回response呢？

其实是用动态绑定实现的。在handler中，这些动态绑定的变量都是可用的，比如：

	(current-request)

这样就可以拿到request信息了，包括请求的路径，请求的参数等等。


	(send-response :body body)

这样子，基本的web处理的功能就都有了。

## 内容处理

### sxml做模板

要说用scheme写web有什么优势，绝对得拿sxml说事！我们看两段代码：

	<html xmlns="http://www.w3.org/1999/xhtml"
		xml:lang="en" lang="en">
		<head>
		<title>An example page</title>
		</head>
		<body>
			<h1 id="greeting">Hi, there!</h1>
			<p>This is just an &gt;&gt;example&lt;&lt; to show XHTML &amp; SXML.</p>
		</body>
	</html>

再看这段代码：

	(html (@ (xmlns "http://www.w3.org/1999/xhtml")
		(xml:lang "en") (lang "en"))
		(head
		(title "An example page"))
		(body
			(h1 (@ (id "greeting")) "Hi, there")
			(p "This is just an >>example<< to show XHTML & SXML.")))

两者是完全等价表示！得益于scheme对s表达式强大的操作能力，sxml写模板非常的自然。

从sxml生成html，我使用了[sxml-transforms](http://wiki.call-cc.org/eggref/4/sxml-transforms)这个egg。

### markdown解析

现在有了路由，有了handler，有了sxml的模板，我需要把markdown格式的文章填充进来。

嗯，解析markdown生成sxml，也有相应的egg可以实现，[lowdown](http://wiki.call-cc.org/eggref/4/lowdown)就是这样一个库。

注意用lowdown的时候还有一点点小问题，它默认是英文字符集的，不支持中文输出。所以我改了一下源代码，加个utf8支持。

### atom

为了输出rss订阅，我又去找了一个生成atom的包，这个egg直接就叫做[atom](http://wiki.call-cc.org/eggref/4/atom)。

-----------

之前一直偷懒，这次重写顺带把评论的功能加上去了，disqus做的。

还有运维方面一些变化，之前Go版本的博客是托管在heroku上面的，每次git push代码，那边会自动重新编译，重启。这次打算丢到自己的vps上面去，一直是markdown格式来写博客，直接git push的方式发布。现在没有推代码自动重编译了，没有重启和重新加载数据，所以呢，就改改代码周期性的读文件重建索引吧。

## 结语

chicken scheme确实是一个非常不错的scheme的实现，它的实现方式非常hack-friendly，而且周边的生态都很好，库也很丰富。

这次拿它来重写博客练手，也算是用scheme语言做一些“实际”的东西吧。

代码还是在 [https://github.com/tiancaiamao/go.blog](https://github.com/tiancaiamao/go.blog)

哦，下次我想做一个web的性能测试，对比一下Go和chicken scheme谁更高一些。
