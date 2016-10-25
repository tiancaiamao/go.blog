[上一次](http://www.zenlife.tk/an-intro-to-modern-opengl-2.md)，我们得到了一个打开的窗口并等待渲染我们的hello world程序的指令。但是在我们实际画任何东西之前，我们必须通过创建各种各样的对象并将它们作为数据传给OpenGL。让我们过一遍我们需要设置的对象：

## 再看管线

![](http://duriansoftware.com/joe/media/gl2-pipeline-01.png)

回顾一下我们第一章中的[图像管线]()，这次从我们的"hello world"程序的视角，我们需要哪些对象就很清晰了。从输入结束开始，我们的顶点数组包含四个顶点，顶点着色器将会把它们赋值给窗口的各个角。元素数组会将这四个顶点组合成两个三角形，形成一个覆盖窗口的矩形。我们会创建一些小的缓冲对象来将这些数组存储在显存中。我们 uniform 状态将由我们的两个"hello"图片以及用于将它们混色的因子组成。这些图片每个需要一个纹理对象。除了将我们的顶点映射到屏幕的角上，顶点着色器还会将一系列的纹理坐标赋值给每个顶点，将顶点映射到它所对应的纹理的角上。然后光栅化过程将会使用纹理坐标对矩形区域表面进行插值，这样，最终我们的像素着色器可以对两个纹理进行取样并将它们使用一个混合因子进行混色。为了将着色器加入到OpenGL里面，我们创建一个program对象来将顶点着色器和像素着色器链接起来。在这篇文章中，我们将设置好缓冲对象和纹理对象；下一次，我们将操作着色器。

## OpenGL中的C类型

OpenGL定义了它自己的跟标准C类型相对应的GL*类型：GLubyte，GLbyte，GLushort，GLshort，GLuint，GLint，GLfloat和GLdouble。OpenGL还提供了一些更具有语义的类型定义：

* GLchar*，用于处理以null结束的ASCII字符串
* GLclampf和GLclampd，它们只是GLfloat和GLdouble的typedef，但是用于表示范围在0到1之间的值
* GLsizei，是整型的typedef，用于表示内存块的大小，类型于标准C库中的size_t
* GLboolean，是GLbyte的typedef目的是存GL_TRUE或者GL_FALSE，类似于C++或者C99中的bool
* GLenum，是GLuint的typedef用于存一个预定义的 GL_* 常量
* GLbitfield，又是一个GLuint的typedef，用于存位组或者一个或多个GL_*_BIT mask

## 存储我们的资源

	static struct {
	    GLuint vertex_buffer, element_buffer;
	    GLuint textures[2];

	    /* fields for shader objects ... */
	} g_resources;

在这里，使用一个像g_resources这样的全局结构体变量用于在我们的初始化代码和GLUT回调之间共享数据是最简单的。OpenGL使用GLuint值作为对象的句柄。我们的g_resources结构体中包含两个GLuint域，我们将用它存放我们的顶点名和缓冲对象的元素数组。我们将添加更多的域来存放我们的着色器对象，当我们在下篇文章中创建它们时。

## OpenGL对象模型

OpenGL操作对象的约定有点不同寻常。你可以通过使用`glGen*s`函数(例如glGenBuffers或者glGenTextures)来创建一个或多个对象。正如前面提到的，得到的句柄是GLuint值。任何由对象所拥有或者关联的数据都是由OpenGL内部管理的。这是很典型的。你如何使用这些句柄就是不一样的地方：为了操作一个对象，你先要通过调用相应的glBind*函数(glBindBuffer或者glBindTexture)绑定到一个OpenGL定义的目标。然后你将target作为参数提供给OpenGL调用，这个OpenGL调用会设置属性或者上传数据到绑定的对象中。目标绑定还影响到一些不显示使用目标作为参数的相关的OpenGL调用，后面我们讨论渲染的时候会看到的。现在，我们看看创建完缓冲对象的模板是什么样子的:

## 缓冲对象

	static GLuint make_buffer(
	    GLenum target,
	    const void *buffer_data,
	    GLsizei buffer_size
	) {
	    GLuint buffer;
	    glGenBuffers(1, &buffer);
	    glBindBuffer(target, buffer);
	    glBufferData(target, buffer_size, buffer_data, GL_STATIC_DRAW);
	    return buffer;
	}

缓冲对象是交给OpenGL管理的内存。它们用于存储顶点数组(使用GL_ARRAY_BUFFER)和元素数组(使用GL_EMEMENT_ARRAY)。当你使用glBufferData分配一个缓冲时，你提供一个使用提示来表明你想要改变缓冲中数据的频率，OpenGL将基于这个提示决定最好是将它的数据存储在CPU还是GPU。这个提示实际上并不会限制缓冲的使用方式，但是以与提示不符的方式去使用会导致性能低下。在我们的程序中，我们的顶点和元素数组都是常量，不需要改变，因此我们给了glBufferData一个GL_STATIC_DRAW的提示。其中STATIC部分表明我们不会想去改变数据。缓冲的提示还可以设置为DYNAMIC，表明我们频繁地写到这个缓冲里，或者STREAM，表明我们将周期性地替换掉缓冲的内容。DRAW部分表明我们希望缓冲只会被GPU读取。与DRAW相对的是READ，表明一个缓冲主要会被CPU读回去，还有COPY，表明这个缓冲是CPU和GPU之间的一个管道，不应该偏重于任一方。顶点数组和元素数组几乎总是使用GL_*_DRAW提示。

	static const GLfloat g_vertex_buffer_data[] = { 
	    -1.0f, -1.0f,
	     1.0f, -1.0f,
	    -1.0f,  1.0f,
	     1.0f,  1.0f
	};
	static const GLushort g_element_buffer_data[] = { 0, 1, 2, 3 };

`glBufferData`看待你的数据源很类似memcpy：仅仅就是一串没有特别意义的字节流。直到我们渲染它们之前，我们不会告诉OpenGL我们数组的结构。这允许缓冲以几乎任何格式存储顶点属性以及其它数据，或者同一份数据给不同的渲染任务以不同的方式去处理。在这里，我们仅仅是以四个两元素向量的集合指定我们的矩形的角。

![](http://duriansoftware.com/joe/media/gl2-vertex-array-01.png)

我们的元素矩阵也很简单，一个`GLushorts`数组依次索引四个顶点元素，这样就可以将它们汇编成一个矩形的三角形带。在桌面版OpenGL，一个元素数组可以由8位GLubyte，16位GLushort，或者32位GLuint成员组成；对于OpenGL ES，只可以使用GLubyte或者GLushort。我们现在像下面这样在我们的`make_resource`中调用`make_buffer`来分配和填充我们的缓冲：

	static int make_resources(void)
	{
	    g_resources.vertex_buffer = make_buffer(
	        GL_ARRAY_BUFFER,
	        g_vertex_buffer_data,
	        sizeof(g_vertex_buffer_data)
	    );
	    g_resources.element_buffer = make_buffer(
	        GL_ELEMENT_ARRAY_BUFFER,
	        g_element_buffer_data,
	        sizeof(g_element_buffer_data)
	    );
	    /* make textures and shaders ... */
	}

## 纹理对象

	static GLuint make_texture(const char *filename)
	{
	    GLuint texture;
	    int width, height;
	    void *pixels = read_tga(filename, &width, &height);
		
	    if (!pixels)
	        return 0;

就像我在上篇文章中提到的，我使用[TGA格式](http://en.wikipedia.org/wiki/Truevision_TGA)来存储我们的"hello world"图片。我不会在这里浪费时间分析代码；如果你想看它的话，它在Github仓库的[util.c](http://github.com/jckarter/hello-gl/blob/master/util.c)。TGA的像素数据以顺序的，未压缩的三字节RGB一组打包的数组存储(实际上是以BGR的顺序)，像素的顺序是从图片的左下角开始，然后从那里向右，再然后向上。接下来我们将看到，这种格式用于OpenGL纹理非常好。如果读图片失败，我们返回0，它是绝不会被真正的OpenGL对象使用的"空对象"名字。

	glGenTextures(1, &texture);
    glBindTexture(GL_TEXTURE_2D, texture);

纹理对象提供处理结构化数组的GPU内存专门用于存储纹理数据。OpenGL支持多种类型的纹理，每种都有它自己的纹理目标，包括1D(GL_TEXTURE_1D)，2D(GL_TEXTURE_2D)和3D(GL_TEXTURE_3D)纹理。还有一些更特殊的纹理类型我们在接下来可能会遇到。2D纹理目前是最常见的类型。这里我们为我们的图片生成并绑定一个GL_TEXTURE_2D纹理。纹理对象和缓冲对象不同，因为GPU处理纹理内存和缓存内存有着很大的着别。

## 纹理取样和纹理参数

顶点数组是一次一个元素地提供给顶点着色器，并且顶点着色器没有任何方式访问到其它的元素。然而在顶点着色器或者像素着色器的任何调用中，整个纹理内容都是可用的。着色器在一个或多个浮点数纹理坐标中取样。纹理数组中的元素均匀地分布到纹理空间中，纹理空间是一个正方型的坐标跨度从（0,0）到(1,1)(或者一个0-1的线性划分，对于1D纹理，或者是一个正方体的划分从（0,0,0)到(1,1,1)对于3D纹理)。为了和对象空间的x,y,z坐标进行区分，OpenGL像纹理空间的坐标轴标记为s,t,r。纹理空间均匀地分布在轴线上形成矩形的单元格，与原数组的宽高一至。格子边界(0,0)映射到纹理空间的第一个元素，随后的元素沿s坐标轴和t坐标轴向右和向上分布。在这些格子中心对纹理进行取样得到相应的纹理数组中的元素。

![](http://duriansoftware.com/joe/media/gl2-texcoords-01.png)

注意，t坐标轴可以被看作向上或者向下（事实上或者是任何方向）增长的，依赖于底层数组表示。纹理空间的另外一个坐标轴也同样是任意的。由于TGA图片将它的像素自左向右，自下向上的存储，这就是我所描绘的坐标轴。

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S,     GL_CLAMP_TO_EDGE);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T,     GL_CLAMP_TO_EDGE);
	
如何对纹理格子之间的纹理，或者是坐标在0-1范围之外的纹理进行取样，是由`glTexParameteri`函数的**纹理参数**控制的。

参数 GL_TEXTURE_MIN_FILTER 和 GL_TEXTURE_MAG_FILTER 分别控制当分辨率高于或低于纹理自身的分辨率时，落于样本的像素点之间的取样。我们将它们设置为 GL_LINEAR 来告诉GPU我们使用线性插值来对最接近取样点的四个点进行平滑的混色。如果用户改变窗口大小，纹理图片将平滑地缩放。设置填充参数为GL_NEAREST将告诉GPU返回离取样点最近的纹理元素，这会导到缩放时像素缩放时会有锯齿。

![](http://duriansoftware.com/joe/media/gl2-texture-filter-01.png)


参数 GL_TEXTURE_WRAP_S 和 GL_TEXTURE_WRAP_T 控制当坐标超出坐标轴中0-1范围时如何处理；在这里，我们不打算对范围之外进行取样，因此我们使用GL_CLAMP_TO_EDGE，它将坐标限制在(0,0)到(1,1)。如果一个或者两个坐标轴的参数是GL_WRAP将造成纹理图片在纹理空间中沿坐标轴无限地重复。

如果抽象地说，纹理取样可能听起来就像复杂的2D数组索引。如果我们看一下我们的像素着色器是如何采样纹理的可能会更有意义：

![](http://duriansoftware.com/joe/media/gl2-texture-rasterization-01.png)

在我们的顶点着色器中，我们会将纹理空间的角赋值给我们的矩形顶点。当光栅化的矩形的大小和纹理大小匹配时（也就是，我们的窗口大小和图片大小一致），片元着色器会一像素一像素地取样，正如左图中你所看到的。如果矩形的光栅化大小和纹理不匹配，每个片元将会在我们的纹理格子中心取样，线性滤波将使我们在纹理元素之间得到一个平滑的梯度，正如右边所示。

## 分配纹理

	    glTexImage2D(
	        GL_TEXTURE_2D, 0,           /* target, level of detail */
	        GL_RGB8,                    /* internal format */
	        width, height, 0,           /* width, height, border */
	        GL_BGR, GL_UNSIGNED_BYTE,   /* external format, type */
	        pixels                      /* pixels */
	    );
	    free(pixels);
	    return texture;
	}

`glTexImage2D`(或者-1D或-3D)函数为纹理分配内存。纹理可以有多个levels of detail，当从更低分辨率取样时可以依次从更小的"[mipmaps](http://en.wikipedia.org/wiki/Mipmap)"层次中取样，但是在这里我们只是提供基本的第0级。不像`glBufferData`，`glTexImage2D`要求对分配内存的所有的格式信息预先提出。internal format参数告诉GPU每个纹理元素使用的颜色组分，以及以什么样的精度存储。OpenGL支持各种的不同图片格式；这里我将只提一下我们所使用的。我们的TGA文件使用24位的RGB像素，换句话说，每个像素由三个8位组成。这个对应于`GL_RGB8`内部格式。宽度和高度参数指定纹理元素在s和t坐标轴上的数目(border参数是废弃的并且总是应该设置为0)。外部格式和类型参数声明了我们的像素的组成顺序和类型，我们的像素指向一个width*height打包的特定格式的纹理元素。TGA以BGR顺序采用unsigned byte存储它的像素，因此我们的外部格式参数使用GL_BGR，类型使用GL_UNSIGNED_BYTE。

让我们在我们的`make_resources`函数中添加一些`make_texture`调用来创建我们的纹理对象：

	static int make_resources(void)
	{
	    /* ... make buffers */
	    g_resources.textures[0] = make_texture("hello1.tga");
	    g_resources.textures[1] = make_texture("hello2.tga");
	
	    if (g_resources.textures[0] == 0 || g_resources.textures[1] == 0)
	        return 0;
	    /* make shaders ... */
	}
	
## 接下来，着色器

我们现在已经准备好我们的顶点和图片数据了，并且准备好启动我们的绘图管线。下一步将是写着色器来通过GPU操控数据并将它加载到屏幕上。这将是我们这一章的下一部分要做的。

[<< 第二章](an-intro-to-modern-opengl-2.md) | [目录](an-intro-to-modern-opengl-0.md) | [下一节 >>](an-intro-to-modern-opengl-2-2.md)
