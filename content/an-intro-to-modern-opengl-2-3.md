现在在我们的["hello world"程序](an-intro-to-modern-opengl-2.md)中，我们已经[加载了我们的缓冲和纹理](an-intro-to-modern-opengl-2-1.md)，并且[编译和链接了我们的着色器程序](an-intro-to-modern-opengl-2-2.md)。终于到最后一步了--让我们来渲染我们的图片。

## 渲染作业综述

渲染可能需要很多的参数。除了所有的缓冲，纹理，着色器，以及它所涉及到的uniform参数，还有许多的其它控制渲染作业的设置我没提到。OpenGL的方法是将这些设置做成了一个状态机，而不是提供一个完整的带所有标记作为参数的"draw"函数，或者一个需要你去填充各个域的结构体。当你使用`glBindTexture`,`glBindBuffer`以及类似的方法绑定一个对象的时候，你不仅是使这些对象可以修改，你还将它们绑定到了当前渲染作业的状态。并且有状态操作函数可以设置当前着色器，赋值到uniform参数和描述顶点数组的结构。当你最后使用`glDrawElements`将一个作业提交时，OpenGL取当前状态机的一个快照并将它添加到GPU的命令队列，它将在当GPU可用时被执行。同时，你可以改变OpenGL状态以及将更多任务加到队列中，而不用等待之前的作业完成。一旦你将作业排队完毕，你可以让窗口系统"切换缓冲"，这个操作将会等待所有的排队作业完成然后将结果显示在窗口中。

让我们写一些代码设置渲染作业状态：

## 激活着色器程序并赋值uniform

	static void render(void)
	{
	    glUseProgram(g_resources.program);
	    
我们首先通过传递链接的程序对象的名字给`glUseProgram`来激活我们的着色器对象。一旦程序激活，我们可以开始对我们的uniform变量进行赋值。如果你回忆下我们的[片元着色器的代码](http://www.zenlife.tk/an-intro-modern-opengl-2-2.md)，我们需要给`float fade_factor`和一个叫做`textures`的sampler2D数组进行赋值。

	glUniform1f(g_resources.uniforms.fade_factor, g_resources.fade_factor);

OpenGL提供了一组glUniform*函数用于给uniform变量赋值，其中每一个对应GLSL程序中的一种uniform变量类型。这些函数都是`glUniform{dim}{type}`的形式，其中`dim`表示vector类型的大小(int或float的uniform是1，vec2是2，等等)，`type`表示组元的类型：要么是i表示integer，要么是f表示float。我们的fade_factor uniform是一个简单的float，因此我们通过调用glUniform1f给它赋值，传入uniform的位置以及新的值作为参数。

 glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, g_resources.textures[0]);
    glUniform1i(g_resources.uniforms.textures[0], 0);

	   glActiveTexture(GL_TEXTURE1);
	   glBindTexture(GL_TEXTURE_2D, g_resources.textures[1]);
	   glUniform1i(g_resources.uniforms.textures[1], 1);
    
 将纹理赋值给samplers有一点点复杂。GPU只有数量有限的纹理单元可以提供给纹理数据给渲染作业。我们必须将我们的纹理对象绑定到这些纹理单元，然后将纹理单元的索引赋值给我们的sampler uniform变量，如果它们是int的话。我们绑定的`GL_TEXTURE_*`目标名必须对应于sampler uniform的类型。在这里，GL_TEXTURE_2D对应于我们的textures变量使用的sample2D类型。`glActiveTexture`设置当前活跃的纹理单元。`glBindTexture`其实是使用活跃纹理单元作为一个隐含参数(其它的纹理对象操作的函数像`glTexParameteri`和`glTexImage2D`也是操作绑定到当前活跃的纹理单元的纹理)。一旦我们绑定纹理单元之后，我们可以使用`glUniform1i`对它的索引进行赋值。
 
## 设置纹理数组
 
		glBindBuffer(GL_ARRAY_BUFFER, g_resources.vertex_buffer);
	    glVertexAttribPointer(
	        g_resources.attributes.position,  /* attribute */
	        2,                                /* size */
	        GL_FLOAT,                         /* type */
	        GL_FALSE,                         /* normalized? */
	        sizeof(GLfloat)*2,                /* stride */
	        (void*)0                          /* array buffer offset */
	    );
	    glEnableVertexAttribArray(g_resources.attributes.position);
	    
接下来，我们告诉OpenGL我们使用的纹理数组的格式。我们通过调用`glVertexAttribPointer`设置每一个顶点属性格式，这个函数告诉OpenGL在渲染时从顶点数组中读出属性值。`glVertexAttribPointer`使用属性位置，关联的属性变量的元素大小和类型(对于我们的position属性，大小为2,类型为GL_FLOAT)，属性值之间的字节数(称为stride)，以及当前第一个属性在当前绑定的GL_ARRAY_BUFFER中的偏移作为参数。由于历史原因，offset是作为一个指针传递的，但它实际上被当作integer值使用，因此我们传递一个整形的0并传换为void*类型。

![](http://duriansoftware.com/joe/media/gl2-vertex-attrib-array-01.png)

在我们这里，我们的顶点数组只由单个vec2 position属性组成；如果我们有多个属性值，属性值可以是交错的，像是一个结构体数组，或者是分别存储在不同的数组里。灵活的`glVertexAttribPointer`让我们可以选择这两种情况中每个属性如何选择stride和offset去适应它们的存储布局；改变GL_ARRAY_BUFFER绑定不影响由我们已经设置过的属性数组指针使用的缓冲。

(上面我没有提到的normalized?参数是跟顶点数组中的整型的数组一起使用的。如果为true，元素将从它们的integer类型的范围进行映射，比如0-255用于unsigned byte，0.0-1.0用于符点数，像图片中的颜色组分。如果为false，它们的整型值将被保存。像我们这样使用的已经是符点数的元素，该参数没有任何作用。)

## 提交渲染作业

	glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, g_resources.element_buffer);
	    glDrawElements(
	        GL_TRIANGLE_STRIP,  /* mode */
	        4,                  /* count */
	        GL_UNSIGNED_SHORT,  /* type */
	        (void*)0            /* element array buffer offset */
	    );
	    
`glDrawElements`是设置绘图管线动作为函数。我们告诉它我们使用哪种三种角组装模式，使用多少顶点组装三角形，我们元素数组的组成类型，以及当前绑定的第一个要渲染的元素在GL_ELEMENT_ARRAY_BUFFER内部的偏移，这也是一个实际上是integer的指针参数。它将获取指向的元素数组的索引，将它们跟当前绑定的着色器程序，uniform变量，纹理单元，我们刚刚设置的顶点属性指针集合在一起，绑定成为一个渲染作业，并将这个作业放到GPU队列中。

## 清理工作

	glDisableVertexAttribArray(g_resources.attributes.position);	
	
"Always leave things the way you found them"，Bill Brasky曾经建议过。OpenGL状态机的缺点就是所有的绑和设置都是全局地持久的，即使调用`glDrawElements`之后。这意味着我们必须注意整个程序生命期中，我们的OpenGL代码是怎样和其它的OpenGL代码交互的。尽管在这个程序中还没有其它的OpenGL代码与之交互，我们仍然应该养成一个好的习惯。尤其要注意顶点属性：在涉及到多个着色器程序和多个顶点数组的复杂程序中，不正确地使用顶点属性可能会造成`glDrawElements`去使用无效的GPU数据，导致错误的输出或者段错误。只在需要的时候去使用顶点数组是一个好习惯。这里，我们对position禁用顶点属性。

你也可能会想，每次渲染时，我们重新绑定了所有相同的对象，设置了所有的相同的uniform值(除了fade_factor)，并且重新激活了所有的同样的顶点属性。如果状态设置在`glDrawElements`调用之间是持久的，从技术上讲在进入`glutMainLoop`之后，我们可以几乎完全没必要要每帧都进行设置，并且每次渲染只更新混色因子并调用`glDrawElements`。但是，在你每次期望的时候都设置好状态，这是个好主意。

## 显示我们完成的场景

	    glutSwapBuffers();
	}

我们只有一个渲染作业需要等待，因此当我们提交作业并清理之后，我们可以立即执行同步。GLUT函数`glutSwapBuffers`等待所有的运行中的作业完成，然后用我们的双缓冲的framebuffer交换颜色缓冲，在下一帧时将当前可见的缓冲移到要渲染的"后面"，然后将我们刚刚渲染好的图象推到前面，在我们的窗口中显示新渲染好的场景。我们的渲染流程完成了！

## 让场景动起来

	static void update_fade_factor(void)
	{
	    int milliseconds = glutGet(GLUT_ELAPSED_TIME);
	    g_resources.fade_factor = sinf((float)milliseconds * 0.001f) * 0.5f + 0.5f;
	    glutPostRedisplay();
	}
	
为了让图片动起来，我们的`glutIdleFunc`回调函数不停地更新我们给fade_factor赋值的uniform。GLUT维护一个毫秒级的计时器，我们可以使用glutGet(GLUT_ELAPSED_TIME)访问到；我们使用标准C语言的sinf函数来得到一个平滑的，周期性的0到1之前的数。每次我们更新混色因子，我们调用`glutPostRedisplay`，这会强制我们的渲染回调函数去执行，更新窗口。

## 再次编译运行程序

这是我们最后一次编译和运行整个程序，使用所有我们的新的代码。构建和执行的命令看起来很像[上次我们构建的空函数版本](http://www.zenlife.tk/an-introduce-to-opengl-2.md)，但是这次，你将编译真正的hello-gl.c和util.c源文件。如果你使用Makefiles，你可以这样编译默认的目标：

	make -f Makefile.MacOSX # or Makefile.Unix or Makefile.Mingw

	nmake /f Nmakefile.Windows
	
一旦编译后，程序假定它的图片和着色器资源是在当前目录的，因此最好从包含可执行文件，图片，着色器代码的目录用命令行运行它。最后我们终于可以晒一下我们的成果了：

![](http://duriansoftware.com/joe/media/gl2-screenshot.png)

## 结论

必须承认从一个简单的"hello world"已经起了很远了。但是这里我们所创建的框架是非常灵活的；你可以替换成你自己的图片并调整着色器代码在图片取样之前对它们进行变换或者进一步处理，都不需要重新编译C。下一章中，我们将继续顶点着色器来展示基本的3D变换和投影。

如果你很感兴趣，这个时候你也可以停下来，自己看一下[OpenGL标准](http://www.opengl.org/registry/)，注意，OpenGL 2标准仍然包含了很多我没有提到的过时的特性。我强烈推荐你看OpenGL 3.1之后的版本，一定要看看核心标准部分而不是为了兼容的部分。尽管OpenGL 3之后相对于OpenGL 2添加了很多新的特征，所有的OpenGL 2中的基本的API也仍然是新版本的基本部分。

[OpenGL ES](http://www.khronos.org/registry/gles/) 2也是值得一看的。它大部分由我这里提到的OpenGL 2之后的一个子集；所有的我前面提到的OpenGL API也都是在OpenGL ES 2中的。OpenGL ES还对移动平台添加了一些额外的特性，比如浮点数支持以及离线着色器编译，这是桌面版标准中所有提供的。如果你想试一下OpenGL ES开发，它是Android NDK和iPhone SDK的部分。在Windows下，Google的[ANGLE项目](http://code.google.com/p/angleproject/)还提供一个OpenGL ES2在DirectX上的实现。

[< 上一节](an-intro-to-modern-opengl-2-2.md) | [目录](an-intro-to-modern-opengl-0.md) | 第三章>
