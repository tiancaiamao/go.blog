## 第二章：Hello World幻灯片

在[前一章](an-intro-to-modern-opengl-1.md)中，我给出了图像管线的整体概述。现在是时候将它付诸行动了。在我尝试渲染任何有趣的3D场景之前，我将像标准的教程那样使用一个简单的，二维的"hello world"程序来展示基本的OpenGl API。我们将使用下面这张图：

![](http://duriansoftware.com/joe/media/gl2-hello-1.png)

将它画到一个大小合适的窗口中。但是静态图片看起来很无趣--我们将它弄得有趣一点，让它和下面这张图一起进行反复地淡入淡出：

![](http://duriansoftware.com/joe/media/gl2-hello-2.png)

尽管仍然不是一个很有趣的程序，但是它非常简单，更复杂的程序使用的OpenGL特性它几乎都会使用到。完整的代码上传到了Github[这里](http://github.com/jckarter/hello-gl)。仅仅将图片画到屏幕上，这个程序使用了380行C代码以及一些的着色器代码，看起来有点过。不过，它将是接下来的一些更有趣的demo的基础。源文件hello-gl.c中包含了OpenGL渲染的代码部分，而util.c中包含了很无聊的公共函数用于读取TGA格式图像。我在其中包含了两个这种格式的图像，hello1.tga和hello2.tga，因为这种格式很容易解析而不用依赖外部库。我们的着色器代码在两个文件中：hello-gl.v.glsl用于顶点着色器，hello-gl.f.glsl用于像素着色器。

在这一章中，我将解释hello-gl程序的各个部分是如何使用OpenGL API将数据放到图像管线中以及如何让它运行起来。当我们讨论着色器时，我还将给出一个简明的GLSL语言的概述。将这些东西全部放在一篇文章中有点多，所以我将这一章拆成四个部分。在这第一部分中，我们将通过GLUT打开一个窗口。在第二部分中，我们将设置我们程序中使用的缓冲区以及纹理对象，它们包含了原始的顶点和图像数据。之后，我们将写着色器代码来处理这些数据将我们最终的图像显示在屏幕上。既然我们的游戏的计划已经布置好了，下面让我们演员们登场。我们首先设置好GLUT并在屏幕中创建一个空的窗口。

### OpenGL头文件


	#include <stdlib.h>
	#include <GL/glew.h>
	#ifdef __APPLE__
	#  include <GLUT/glut.h>
	#else
	#  include <GL/glut.h>
	#endif


不同的平台将它们的OpenGL头文件放在不同地方，但是有了GLEW，你不需要担心这些。不管它们在哪里，包含GL/glew.h将会为你包含进系统的OpenGL头文件。不幸的是，包含GLUT仍然有些坑，需要你手动设置一些跨平台的东西。它的头文件一般在GL/glut.h，但是MacOS X的附带的GLUT框架使用苹果自己的头文件约定，将GLUT头文件放在GLUT/glut.h。最近版本的Visual Studio的标准C头文件与glut的交互方式有个bug，导致包含stdlib.h的顺序要在glut.h之前。(译者注：原文时间约为2010年4月，我不确定现在版本的VS是否还有这个问题)

### 用GLUT设置好我们的窗口


	int main(int argc, char** argv)
	{
	    glutInit(&argc, argv);
	    glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE);
	    glutInitWindowSize(400, 300);
	    glutCreateWindow("Hello World");
	    glutDisplayFunc(&render);
	    glutIdleFunc(&update_fade_factor);


GLUT提供了一个有限制的，但是直观并且可移植的窗口系统接口。通过调用glutInit准备好GLUT之后，我们使用glutInitDisplayMode来指定我们默认的framebuffer应该使用怎样的缓冲区。在这里，一个双缓冲(GLUT_DOUBLE)的颜色缓冲区(GLUT_RGB)足够了。([Double buffering](http://en.wikipedia.org/wiki/Double_buffering#Double_buffering_in_computer_graphics)为framebuffer提供了两个颜色缓冲区，每一帧中一个用于显示在屏幕上，另一个画，在两者中切换，这样动画看起来就是平滑的)。如果我们需要深度或者 stencil 缓冲区，我们可以在这里做。然后我们使用glutIninWindowSize设置初始的图片的窗口大小为400x300并使用glutCreateWindon创建窗口。最后，我们指定两个回调函数接受窗口事件：一个glutDisplayFunc用于在窗口需要显示时渲染我们的图像，一个glutIdleFunc持续地更新两个图片之间随时间渐变的因子。


	glewInit();
	if (!GLEW_VERSION_2_0) {
	        fprintf(stderr, "OpenGL 2.0 not available\n");
	        return 1;
	}


在GLUT创建我们的窗口之后，它会准备好OpenGL，这样我们就能开始调用这个库。我们首先要做的是初始化GLEW。当函数glewInit被调用时，它会根据OpenGL的可用版本以及扩展设置一系列的标志。这里在继续之前我们检测GLEW_VERSION_2_0标记以确保OpenGL 2.0是可用的。除了设置版本标记，GLEW几乎不扮演其它任何角色，并且在它初始化之后我们不需要与它交互。


	if (!make_resources()) {
	    fprintf(stderr, "Failed to load resources\n");
	    return 1;
	}

    glutMainLoop();
    return 0;
	}


GLEW初始化后，我们调用我们的make_resources函数来设置我们的OpenGL资源。如果我们的资源加载成功，glutMainLoop会运行。它显示窗口，开始从窗口系统接收UI事件，并调用我们设置好的回调函数响应这些事件。它还会在用户退出时帮我们退出程序。return 0仅仅是避免编译警告，实际上绝对不会运行到那里。

### 编译并运行我们的程序

现在我们可以先不管GLUT回调函数和make_resources函数，得到一个可以运行的，尽管没什么价值的程序：


	static int make_resources(void)
	{
	    return 1;
	}
	static void update_fade_factor(void)
	{
	}
	static void render(void)
	{
	    glClearColor(1.0f, 1.0f, 1.0f, 1.0f);
	    glClear(GL_COLOR_BUFFER_BIT);
	    glutSwapBuffers();
	}


glClearColor设置一个RGBA消除颜色(这里是白色)，然后glClear使用这个颜色来填充framebuffer的颜色缓冲区。glutSwapBuffers然后将我们的用于消除的颜色缓冲区显示在屏幕上。通过这几个函数，现在我们可以编译和运行我们的程序。这个空函数版本在Github仓库中对应的是hello-gl-dummy.c。编译程序并链接到OpenGL，GLUT和GLEW库的命令在不同平台下会有些区别。在大多数类Unix下大概像下面这样子：
	
	gcc -o hello-gl-dummy hello-gl-dummy.c \
	    -I/usr/X11R6/include -L/usr/X11R6/lib \
	    -lGL -lGLEW -lglut

在MacOS X中：


	# Assuming GLEW was installed to /opt/local
	gcc -o hello-gl-dummy hello-gl-dummy.c \
	    -I/opt/local/include -L/opt/local/lib \
	    -framework OpenGL -framework GLUT -lGLEW


在Windows中使用Visual C++:

	cl /Fohello-gl-dummy.obj /c hello-gl-dummy.c
	link /out:hello-gl-dummy.exe hello-gl-dummy.obj \
	    opengl32.lib glut32.lib glew32.lib

在Windows中使用mingw:
	
	gcc -o hello-gl-dummy.exe hello-gl-dummy.c \
	    -lopengl32 -lglut32 -lglew32

在Github仓库中还包含了各个平台下的makefile。你可以使用hello-gl-dummy(或者hello-gl-dummy.exe，在Windows下)编译这个版本的程序:

	make -f Makefile.MacOSX hello-gl-dummy # or Makefile.Unix or Makefile.Mingw
	nmake /f Nmakefile.Windows hello-gl-dummy.exe

一旦你编译好程序，你应该能够运行它并且得到一个白色的窗口：

![](http://duriansoftware.com/joe/media/gl2-dummy-screenshot.png)

关闭窗口，或者在MacOS X下退出应该可以关闭它。

### 接下来，缓冲以及纹理

我们已经准备好了将我们的顶点和图像放到OpenGL。在下一篇文章中，我们会介绍OpenGL的缓冲以及纹理对象。

[<< 第一章](an-intro-to-modern-opengl-1.md) | [目录](an-intro-to-modern-opengl-0.md) | [下一节 >>](an-intro-to-modern-opengl-2-1.md)
