想到一个极简的 lisp 模块实现方案，记录一下。

先说 elisp，elisp 里面是没有 namespace 等等复杂的概念的，不同 package 之间，为了避免命名冲突，就加上很长的前缀。
其实我觉得前缀长一点也没啥的，只是写起来麻烦一点，至少 elisp 也是可以按 package 去组织代码的。

希望写法可以按这样子写，一个文件就是一个模块。文件的路径就是模块的 import path。

被调用的包 `cat /path/to/file1`


```
(defun .f ()
	...)
```

代码实际变成了

```
(defun /path/to/file1.f ()
	...)
```

调用者调用 `cat /path/to/another/file2`

	
```
(import "/path/to/file1" as p)

(defun .g()
	(p.f))
```

代码实际变成了

```
(defun /path/to/file2.g ()
	(/path/to/file1.f))
```

所有的文件内的定义的 symbol，都自动加上了文件的路径，哪怕 symbol 是全局的，由于文件路径的区分，也不会产生模块间的命名冲突。


可以在 reader 那层做点手脚，特殊解析 `p.f` 这样以点分隔的符号，如果文件头有 `import /path/to/file1 as p` 就自动将 `p.f` 转成
`/path/to/file1.f`。简单的讲，就是存在 import 之后，`p.f` 就相当于 `/path/to/file1.f` 的缩写。


实现方式只需要 reader 那边，识别出文件头里面的 import，对后续的词法解析去做替换。

注意到 defun 用到的函数名是 `.f` 和 `.g` 这种，而不是 `f` 和 `g`，这是做一个扩展，`xxx.yyy` 如果 xxx 是空的，就是直接指模块自己，展开时追加的是自身的文件路径前缀。

也就是如果想暴露不带前缀的(global)的，那么还是写成

```
defun f ...
```

而如果是模块内部的，则是写成

```
defun .f ...
```


优点就是这套实现非常简洁，只改改 reader 就完事了。import 变成模块加载都可以直接改写成 load 操作，当然要注意下模块文件只被 load 一次。
