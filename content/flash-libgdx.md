## libgdx的flash工具

libgdx是没有直接支持flash导入的。网上找了找，没有找到现成可用的东西。倒是找到一个很搞笑的帖子，那哥们说他写这工具写了很久都没搞定，工作也丢了，女朋友也没了...听说咱公司以前也让一个实习生做过，最后那实习生快被搞疯了，走了。

flash动画太重要了。目前公司是有做这样的东西的，但是各种不完善。补间动画不支持，透明度不支持，动画中套动画，元件之类的东西也不支持，使用起来各种蛋疼。现在我的游戏的很多特效都是由美工做flash实现的，像光影效果一些，如果不支持flash那就是个大坑了。

我不能容忍咋公司的工具做这么糙，必需改进它。于是我花了点时间研究怎么实现。真的很难么？不至于吧。

## xfl格式解析

xlf是flash的一种导出格式，是xml的，不过也没找到文件格式说明的官方文档。导出目录最重要的两个分别是DOMDocument.xml和Library目录。DOMDocument.xml是存放的对flash内容的描述，Library目录是放的图片资源文件。

xml中记录了时间轴timeline，时间轴上包含了每一层layers，然后每一层中又有帧信息frames，关键帧从第0帧往后。

每一帧frame中，记录了有哪些元素element，并且记录了这些元素做什么样的变换，形变（旋转，缩放），位移，颜色等：matrix，translate，color。这些元素的素材是放在Library文件夹中的。在Library中有对应的png文件和xml文件，xml文件的格式暂时分析不出来，其中有edge什么的。

看一下矩阵变换，可以参考这份资料：

http://www.senocular.com/flash/tutorials/transformmatrix/

其实只要把这个xml格式解析出来，然后用libgdx进行render，就能够还原flash动画了。

## libgdx动画渲染

做成一个animation包，其中Parser类是解析DOMDocument.xml文件的，生成我们使用的数据结构Xfl，这个数据结构控制了内容是怎么样播放的，像第几帧中有哪些元素，对它们做什么操作。

重点是FAnimation类，这个类根据Xfl和资源文件生成动画的数据结构表示，控制动画的播放。动画描述和资源是分离的，只要给它一个标识符到资源的映射关系Map<String, Texture>，好处是可以在加载的时候把资源打包，用TextureAtlas加载。

FAnimation类是参考libgdx库中的Animation类做的，也有控制播放的loop，获取关键键getKeyFrame等函数。不同的是Animation中是直接取得的Texture，而FAnimation中得到的是自己实现的KFrame，KFrame是一个实现了Drawable接口的东西，这样就可以很容易地画出来了。

draw的时候，对一个KFrame，遍历每个图层，对图层中的每个元素进行变换，然后画出来。

下面这段代码是使用例子：

	import com.doodleapp.animation.*; 
	Parser parser = new Parser();
	Xfl xfl = parser.Parse(Gdx.internal.files("DOMDocument.xml"))
	Map<String, Texture> texture; //加载资源并建立好对应关系
	FAnimation animation = new FAnimation(xfl, texture);

最后，东西我开源放到了github，伸手党福利。

http://github.com/tiancaiamao/flash4libgdx
