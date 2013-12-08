# mac的terminal下按键ctrl+/无效

在玩[godit](https://github.com/tiancaiamao/godit)的时候，发现在个问题。按键ctrl+\并没有执行undo。

跑去给人家提[issue](https://github.com/nsf/termbox-go/issues/29)，然后才发现并不是godit的问题。

我跟代码时发现当我按下ctrl+/，终端并没有发送任何内容给termbox-go。这个问题其实是跟终端相关的。ctrl+/并不是一个有效的伪终端字符集，只是emacs中才有的。并且ctrl+_ ctrl+7 ctrl+/实际上发送的都是同一个字符0x1F。

最后换成[iTerm](http://www.iterm2.com)就可以了。
