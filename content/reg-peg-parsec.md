上次年终总结中提到，cora 接下来计划的其中一个方向，考虑像 [babashka](https://babashka.org/) 那样子做成日常的脚本来使用。
如果是当作日常脚本使用，其中很重要的一块是文本处理能力。而说到文本处理，对于正则表达式的支持首先就出现在了脑海里。所以这一篇的话题就讨论正则表达式，PEG 以及 parser combinator。

正则当然可以用一些三方库，但是作为一个完善语言的库的过程，所以我想自己撸一些东西。

正经的正则的实现可以参考 russ cox 写过一系列关于[正则表达式](https://swtch.com/~rsc/regexp/regexp1.html)的文章，做法是编译成 NFA 或者 DFA。
也有一些极简的取巧做法是像《代码之美》的书里面，[有一章](https://www.cs.princeton.edu/courses/archive/spr09/cos333/beautiful.html)专门讲这个话题，如果是 toy 级别的实现，当然也可以这么干。但是其实老实说，我并不太喜欢正则，所以找一找正则表达式的替代品。

然后就回想起 PEG(Parsing Expression Grammars) 了。我记得之前读过 Janet 的一篇[博客](https://bakpakin.com/writing/how-janets-peg-works.html)，是实现 PEG 的，当时觉得特别精妙！所以这个周末我又翻出来拜读了一遍。

把里面的例子 port 成 cora 代码之后，大概就是这样一个函数：

```lisp
(func match-peg
      ['! x] text => (let pos (match-peg x text)
			  (if (> pos 0)
			      0
			      1))
      ['+ x y] text => (let pos (match-peg x text)
			    (if (= pos 0)
				(match-peg y text)
				pos))
      ['* x y] text => (let pos1 (match-peg x text)
			    (if (> pos1 0)
				(let pos2 (match-peg y (string/slice text pos1))
				     (if (> pos2 0)
					 (+ pos1 pos2)
					 0))
				0))
      peg text => (if (string/has-prefix? text peg)
		      (string/length peg)
		      0))
```

`match-peg` 接受一个 PEG 的 pattern，以及输入的字符串，返回匹配到的位置。如果没匹配上，则返回 0。
pattern 有几种基本操作：或，且，非。这里分别是用的 `'+`  `'*`  `'!` 表示的，可以组合起来。

比如说想要匹配数字，这个 PEG 可以写成用"或"处理 0~9 的每一个：

```lisp
(set '<digits> ['+ "0" ['+ "1" ['+ "2" ['+ "3"
   ['+ "4" ['+ "5" ['+ "6" ['+ "7" ['+ "8" "9"]]]]]]]]])
```

然后日期中的年是一个四位数字，就可以用顺序的串起来 4 个 digits：

```lisp
(set '<year> ['* <digits> ['* <digits> ['* <digits> <digits>]]])
```

如果我们想要匹配 "2019-06-10" 这样的日期，完整的 PEG 语法就类似这样子：

```lisp
(set '<digits> ['+ "0" ['+ "1" ['+ "2" ['+ "3"
   ['+ "4" ['+ "5" ['+ "6" ['+ "7" ['+ "8" "9"]]]]]]]]])
(set '<year> ['* <digits> ['* <digits> ['* <digits> <digits>]]])
(set '<month> ['* <digits> <digits>])
(set '<day> <month>)
(set '<iso-date> ['* <year> ['* "-" ['* <month> ['* "-" <day>]]]])
```

然后就可以执行：

```lisp
(match-peg <iso-date> "2019-06-10")
```

demo 的代码在[这里](https://github.com/tiancaiamao/cora/blob/a49e88d286c60ac7a2caa04d70f8e0373506735a/lib/peg.cora)。

如果是像 cora 这种 lisp 语言，弄 PEG 的库，可以利用其代码即数据的能力，去做 DSL。

我突然又联想到，其它的语言，做 PEG 会怎么处理。然后去翻了一下 Go 的。发现 Go 的版本是用了代码生成，像 [peg](https://github.com/pointlander/peg) 或者 [pigeon](https://github.com/mna/pigeon) 都是。可以理解，毕竟 Go 的 DSL 能力是很弱的。但是我还是不太喜欢像 parser generator 这种形式额外引入一套语法，使用起来有学习成本，而且 parser generator 的东西，调试起来特别不方便。

然后又联想到，PEG 其实跟另外一个东西很像 -- parser combinator。

两者的区别是：PEG 实现中，第一个参数是语法规则，第二个参数是输入文本。内部会有解释器根据语法规则，去匹配输入。
而 parser combinator 中，没有解释器了，语法规则和解释器融合到了一起，变成了函数。

我尝试了一下 parser combinator 用 Go 的实现做，首先是定义一个接口：

```Go
type Parser interface {
	Parse(input string) (bool, string)
}
```

然后可以定义一些基础的 parser，比如专门解析字符串常量的 parser：


```Go
type Literal string

func (s Literal) Parse(input string) (bool, string) {
	if len(input) >= len(s) && input[:len(s)] == string(s) {
		return true, input[len(s):]
	}
	return false, input
}
```


接下来是核心部分，组合子。比如说 OR 组合子接受多个 Parser，返回一个新的 Parser。效果是其中某一个 Parser 能解析输入，则处理成功。


```Go
func OR(ps ...Parser) Parser {
	return orC(ps)
}

type orC []Parser

func (c orC) Parse(input string) (bool, string) {
	for _, p := range c {
		succ, remain := p.Parse(input)
		if succ {
			return succ, remain
		}
	}
	return false, input
}
```

SEQ 组合子接受多个 parser，用它们依次按顺序去解析文本。第一个成功后，再用第二个解析剩下的文本。如果中间有失败了，就失败了。
关键点是，通过组合子，能够可以把一些基础的 parser 组合起来，变成处理复杂语法的 parser。还是上面的处理 iso date 的例子，可以这么弄：

```Go
func TestISODate(t *testing.T) {
	var tmp [10]Byte
	for i := '0'; i < '9'; i++ {
		tmp[i-'0'] = Byte(i)
	}
	digits := OR(tmp[0], tmp[1], tmp[2], tmp[3], tmp[4], tmp[5], tmp[6], tmp[7], tmp[8], tmp[9])
	year := SEQ(digits, digits, digits, digits)
	month := SEQ(digits, digits)
	day := SEQ(digits, digits)
	xx := Byte('-')
	date := SEQ(year, xx, month, xx, day)

	input := "2021-10-23"
	succ, remain := date.Parse(input)
	require.True(t, succ)
	require.Equal(t, remain, "")
}
```

Byte 是最基础 parser，通过 OR 组合之后变成可以处理 digits 的 parser，而四个 digits 通过 SEQ 组合之后可以处理 year 格式解析，year / month / day 都定义出来之后，用 SEQ 组合子串到一起就处理 date 格式了。

parser combinator 的组合能力，跟 PEG 的组合能力是一样强大的。都是从最基本的 or, sequence, not 等组合起来。

我试着写了一点代码来解析 Go 的 benchmark 时打印的像这样 "Benchmark FuncnameXXX 23132 ns/op     823032 bytes/op    41 allocs/op" 的文本。


```Go
type BenchResult struct {
	OP    Number
	Byte  Number
	Alloc Number
}

func (r *BenchResult) Parse(input string) (bool, string) {
	WS := WhiteSpace{}
	Benchmark := Literal("Benchmark")
	Funcname := Funcname{}
	NSOP := Literal("ns/op")
	BytesOP := Literal("bytes/op")
	AllocsOP := Literal("allocs/op")
	pattern := SEQ(Benchmark, WS, Funcname,
		WS, &r.OP, WS, NSOP,
		WS, &r.Byte, WS, BytesOP,
		WS, &r.Alloc, WS, AllocsOP)
	return pattern.Parse(input)
}
```

它可以把字符串解析出来，并且把结果(需要的几个字段)存到结构体里面去。

感觉可以。demo 代码在[这里](https://github.com/tiancaiamao/parsec/blob/89f87d9946bb877c526de933d4c779122348aba3/parsec_test.go#L82-L93)。其实关于保存 parser 出来的结果，是有一些技巧性的...不展开了。

结论是：不想用正则的情况下，如果语言对 DSL 的支持比较友好，用 PEG 挺好的。
否则，用 parser combinator 可能会更方便。
