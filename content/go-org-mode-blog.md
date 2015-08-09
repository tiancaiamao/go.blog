## 设计目标

半静态的博客。尽量是self-contain的，没有数据库依赖。只要完全基本功能。使用博文头部有元信息来分类别，tag等，go语言后台动态生成按类别浏览。

静态部分包括：

- css样式文件
- 各种显示的模板
- 大多数博文都是由org文件使用emacs生成的html

动态部分包括：

- 由go语言程序生成rss的feed
- 管理博文的分类，tag，等
- 评论等功能后期再考虑

## 操作流程

手写org格式的博文。文件格式有特殊头部存储博文json格式的meta信息。然后用emacs加工，将org文件转化为html导入到pub/post目录，只生成body部分。
 
go语言写后台，将pub/post目录的文件结合template/post.tpl生成完整的html输出。写博客只要写org文件就行，发布就是用emacs导出html弄到pub/post目录

## 目录格式

生成的博客目录:

	pub
	├── css
	│   └── styles.css
	├── img
	│   ├── back-footer.png
	│   ├── back.png
	│   └── header.jpg
	├── page
	│   ├── about.html
	│   └── index.html
	├── post
	│   ├── beansdb.html
	│   ├── chibi-scheme0.html
	│   └─ chibi-scheme1.html
	│
	└── template
	    ├── archs.tpl
	    ├── categories.tpl
	    ├── index.tpl
	    ├── left.tpl
	    ├── posts.tpl
	    ├── post.tpl
	    ├── right.tpl
	    └── tags.tpl

pub是生成的站点的根目录，其中:

- css放样式文件
- post目录下全是生成的html文件，由org文件用emacs生成,输入格式是body-only，带特殊的json头部
- template是模板文件

源代码的目录：

	src
	└── blog
	    ├── atom
	    │   └── atom.go
	    ├── main.go
	    └── post
		├── post.go
		├── post_test.go
		└── test.html

- atom包是xml的atom feed的结构体定义
- post是实际的web服务器
- main中是空函数，实际是调用post包的init注册消息处理的

博文目录:

	org

这个目录存放写的格式博客文件，都是.org格式的。文件头部有特殊meta信息包括发布日期，分类，tag等。

## 博文头部meta格式

	Title string
	Date Time
	Category []string
	Tags []string
	
一个例子是：{Title:"hello world", Data: 19890-01-07, Category:["life", "test"], Tags:null }

## 功能实现

有几个重要的数据包括:

	var All []*PostData
	var Categories map[string][]*PostData
	var Tags map[string][]*PostData

其中All就是所有的博文。Categories是所有的类别，一个类做为key，对应的value是这一类的博文。Tags跟Categories类似。主页显示上面有一栏Category，实现就是用go的template把Categories的所有keys显示出来。对应的链接是这种形式：
http://my.blog.com/post?category=key  

后台会跟据类别动态生成对应的文章。feed功能的实现就是atom.go中定义了feed的xml数据格式，调用go语言的xml包marshal生成对应的数据。主要就是对All转换以后进行marshal。
