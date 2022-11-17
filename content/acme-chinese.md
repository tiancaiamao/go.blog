我非常喜欢[acme](http://www.zenlife.tk/acme.md)，但是它默认是不支持中文的，花了好大力气才弄好，分享一下。

----------------

为了正常显示中文字符，首先是字体设置。

启动时使用-f选项是使用字体：

	acme -f $PLAN9/font/pelm/song.9.font

自带的字体在font文件夹下。但是默认的字体很丑，而且有些中文还是显示不了的。更好的做法是：
	
	fontsrv &
	9 mount `{namespace}^'/font' mnt/font
	
使用fontsrv服务将系统的字体转转换成plan9识别的格式，然后mount到本地目录，然后使用：

	acme -f mnt/font/STHeiti/16a/font

这样就可以很好地显示中文了。这个字体是我精心挑选的一个，你也可以自己找找觉得满意的。

---------------------

中文输入必须改源码。我在g+上问了russ cox，由于devdraw是处理原始的keycode的，导致acme无法支持输入法。osx版本的devdraw是另一个家伙写的，其实russ cox对osx也不熟。我给作者发邮件没鸟我。最后只好自立更生了。

devdraw是plan9port下唯一直接与X通信的程序，其它都是通过协议调用devdraw实现界面的。不过plan9port中的devdraw只实现了协议部分，并没有实现/dev/draw文件接口。

这里下载一个我[修改过的版本](https://github.com/tiancaiamao/devdraw)，用它替换$PLAN9/src/cmd/devdraw重新编译devdraw就可以了。由于我也完全没接触过objective-c和cocoa，写得比较挫，我在自己使用过程中再慢慢改进它吧。have fun!