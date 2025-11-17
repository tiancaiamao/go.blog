从 gambit scheme 那里学到的。gambit 的编译器支持 namespace，一个符号最终会被加上 namespace## 的前缀。##namespcae 关键字可以决定加上什么样的 namespace 前缀。

下面是规则:

```
(1) (##namespace ("ns#"))
(2) (##namespace ("ns#" name1...))
(3) (##namespace ("ns#" (name1 alias1)...))
```

- 在第一种形式里面，对于这个 namespace 中，遇到任何没有 ## 的符号,会加上 ns# 的前缀。
- 在第二种形式中，只有列表中的比如 name1 会加上 ns# 前缀。
- 在第三种形式，会加上前缀并且进行替换。name1 会被处理成 ns#alias1

文件最开头的顶层的 ##namespace 会对该文件剩下的代码都生效。


```
(##namespace ("math#")
             ("" (def define) if < * -))

(def (fact n)
    (if (< n 2)
        1
         (* n (fact (- n 1)))))
```

根据上面的规则处理之后，就会变成

```
(define (math##fact math#n)
	(if (< maht##n 2)
		1
		 (* math##n (math##fact (- n 1)))))
```

namespace 实现起来是相对比较容易的，可以在 reader 那边维护一层映射，让 reader 去理解 namespace。建立映射规则: "对于 X，加的前缀是 Y"。用一个 map 就可以搞定。比如 reader 在处理完 `(##namespace ("" define - = * if begin set lambda ....))` 之后，就加入到 map

```
define => ""
- => ""
= => ""
begin => ""
set => ""
lambda => ""
...
```

接下来看，如何通过 namespace 来提供 define-library 语法。r7rs 的 define-library 语法长这样子:

```
(define-library (github.com/fred hello)
	(export hi)
	(import (only (scheme base) define)
	(rename (scheme write) (display show)))
	(begin
		(define (hi str)
		(show "hello ")
		(show str)
		(show "\n"))))

(define-library (gitlab.com/zoo cats)
	(import (only (scheme base) define)
		(github.com/fred hello @1.0))
	(begin
	(define (main)
		(hi "lion")
		(hi "tiger"))))
```

把它们翻译到 namespace，相应的写法是:


```
(##namespace ("github.com/fred/hello@1.0#")
			("" define (show display) write-shared write write-simple))

	(define (hi str) ;; defines github.com/fred/hello@1.0#hi
		(show "hello ") ;; calls display
		(show str) ;; same
		(show "\n")) ;; same
		
(##namespace ("gitlab.com/zoo/cats@2.0#")
			("" define)
			("github.com/fred/hello@1.0#" hi))

		(define (main) ;; defines gitlab.com/zoo/cats@2.0#main
			(hi "lion") ;; calls github.com/fred/hello@1.0#hi
			(hi "tiger")) ;; same	
```


可以看出，之前的三条 namespace 规则主要对应于三个场景:

`(##namespace ("ns#"))` 这一个是对应于当前的 library 下面的符号，都默认加上自己的 namespace 前缀。

`(##namespace ("" ("" define (show display))))` 而以 `""` 开头的替换是用于标准库默认的前缀

`(##namespace "github.com/fred/hello@1.0#" hi)` 这种则是对应于导入其它 library 的重命名。我们想使用 hello library 下面的 hi 则会生成这样一条 namespace 规则。


以上。

之前有写过[一个极简(山寨)的 lisp 模块方案](/cora-module.md)，也是 cora 语言当前使用的方案。跟这里这种方案可以做一个简单的比较。

感觉就是 namespace 通用性更强一点，而并没有引入太多的复杂度，只略微比之前那种复杂一点儿。

归根到底，就是我们要有一个规则来决定，对每一个符号，需要加上什么样的前缀。而且分类后可以变成三种：分别是全局的，模块自己的前缀，导入其它模块时的前缀。namespace 方案用了上面的三条规则来定义。而极简模块方案里面，用的是约定什么都不加表示全局； `.` 开头表示模块自己的前缀；用导入其它模块则通过 `(@import "path/to/other/module" xxx)` 来替换。

可以说 cora 中采用的极简方案更加类似于 Go 语言那样，只不过 Go 中是以大小写开头命名来决定一个符号是否从包中导出。而 cora 中则是用了 `.` 的前缀来区分。
