android游戏开发中，铺界面是一个很蛋疼的事情。尤其是当咱公司一直是使用libgdx这种比较原始的工具，配套的基础设施都不怎么强大。粒子编辑器还凑合，像动画编辑器，UI编辑器都没有好用的。没办法，只好自己动手，丰衣足食了。

这次Ui老板不满意，又要大改，实在受不了一遍遍地手动敲坐标了，决定写个库做个gleed2d的解析，然后在libgdx中直接生成Ui。

## gleed文件使用的设计

gleed2d是我们使用的一个铺图的工具，可以直接铺图读坐标，它也会生成一个xml文件包含需要的一些信息。

gleed2d中提供的有用的东西，一个是层的概念，另一个是可以自定义属性。我决定设计成这样：默认一个layer就是一个group，一个TextureItem就是一个image。RectangleItem用于自定义类型。给每个RectangleItem加上一个自定义的type属性。

RectangleItem的自定义类型可以用来做很多事情。比如说我定义Label，可以直接在gleed2d中拉一个矩形，然后定义如下属性：

	RectangleItem name {
		type: label
		text: "这个label中要写的字"
		align: center
		style: fontA
	}

点9图也是使用的RectangleItem，矩形的大小决定了拉伸成的大小，不过自定义属性中多了一项资源位置信息，定义也是类似的:

	RectangleItem item {
		type: dot9
		resource: z:\assets\ui\gamescreen\chuangkou.9.png
	}

怎么用gleed2d提供的layer是同级的结构，如何用它实现Group这种树状的结构呢？我决定用RectangelItem定义一个引用类型，引用到另一个Layer。比如这样：

	Layer layer1 {
		TextureItem blah {
			x: y: w: h:
		}
		TextureItem blaa {
			x: y: w: h:
		}
		...
		RectangleItem rect {
			type: XXX
			ref: layer2
		}
	}

	Layer layer2 {
		...
	}

这样子在layer1中的rect就是一个XXX类型的Group对象，ref属性指定它引用到layer2。

## 库的设计

我同事也有自己写一个gleed2d的libgdx库，本来想直接拿他的用的，可惜跟他项目绑定太紧了。我只好一边跟他说“没关系，其实为自己项目订制的才是最适合的”，一边在心里暗骂，“靠，这贱人完全不考虑重用性的”。不过他的xml解析部分的代码我倒是拿过来用了。

他的做法我不太喜欢，代码定得太死了。比如说他的从gleed的RectangleItem结构中生成相应对象的代码是：

	if ( type == "label") {
		...
	} else if (type == "button") {
		...
	} 

他在项目中自定义了常用的基本对象，像button，label等等这一类，然后将这部分写死了。如果要添加新的类型就需要改这里的代码。我觉得不太好。如果是作为库来写，应该是提供一种生成对象的机制而不是提供很多种预定义的对象。所以我提供的是一个注册函数：

	registerHander(String type, Handler hander)

然后在解析时就是

	Handler handler = getHandler(type)
	Actor actor = handler()

如果你想添加一个新的自定义类型，只需要写一个Handler告诉库如何解析相应的属性字段，生成这个自定义类型对象。给定一个名字，生成该名字的对象，使用了Java中的reflect特性。当然，在库中我也提供好默认的像Label这类常用的基本对象。

其它部分还是比较好弄的，以某个Layer为入口，如果遇到TextureItem则生成图片对象了加入到group，如果遇到的是RectangleItem，则通过type找到对应的handler，然后调用得到相应的actor，加到group中。在自定义的handler中，如果遇到RectangleItem也是递归地进行解析的。

花了两天的时间把这个工作搞完，结果发现并没感受到生产力的大量提升。主要是，以前是铺图，填坐标。现在是铺图，不用填坐标。主要时间其实是花在铺图上面。至于调试，感觉仍然是不方便，以前要改代码坐标，编译，运行，看看对不对。现在是，gleed2d中修改，文件拷过来，重新编译，运行，看看对不对。还变麻烦了，唉...

代码就先不放上来了，等后面项目做完觉得用起来比较稳定再放吧。

----------------------------------------------------

现在已经没有使用这种方式了，换成了代码生成的做法。

后面想了想，浪费一行代码在UI上面都是可耻的。而读取xml然后写库的方式，还是会有些事情要做。并且在解析xml生成对象时会有些限制，比如根据类型名称生成相应的对象就涉及到了Java里的反射或一些相对高端的东西。再比如说直接添加事件这种，都是没法支持的。

真正理想的终极状态是，在编辑器中能给对象添加事件。然后生成好代码，在代码中只需要写事件处理。

2013.11.23更新
