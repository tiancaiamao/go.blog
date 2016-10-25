[缓冲和纹理](http://www.zenlife.tk/an-intro-to-modern-opengl-2-1.md)包含了OpenGL程序所需要的原材料，但是没有着色器，它们只是无效的字节块。如果你还记得我们概要中的[绘图管线](http://www.zenlife.tk/an-intro-to-modern-opengl-1.md)，渲染需要一个顶点着色器将我们的顶点映射到屏幕空间，还需要一个片元着色器，对生成的三角形的光栅化片元进行着色。OpenGL中的着色器是使用一种叫作GLSL(GL Shading Language)的语言写的，它看起来跟C语言很像。在这篇文章中，我们将展示我们的"hello world"程序的着色器代码，然后写C代码来加载，编译并将它链接到OpenGL。

## 顶点着色器

这个是我们的顶点着色器的GLSL代码，在[hello-gl.v.glsl](http://github.com/jckarter/hello-gl/blob/master/hello-gl.v.glsl)中：

	#version 110
	
	attribute vec2 position;
	
	varying vec2 texcoord;
	
	void main()
	{
	    gl_Position = vec4(position, 0.0, 1.0);
	    texcoord = position * vec2(0.5) + vec2(0.5);
	}

我先总结这个着色器做什么事情，然后再给出关于GLSL更多的一些细节。这个着色器首先将顶点的屏幕坐标赋值到`gl_Position`，它是GLSL提供的一个预定义变量。在屏幕空间中，坐标(-1,-1)和(1,1)分别代表framebuffer的左下角和右上角；由于我们的顶点数组也是这样的矩形，我们可以直接拷贝每个顶点`position`值的x和y。`gl_Position`的另外两个向量组成是用于深度测试和透视投影(译者:perspective projection是专业术语吧？如何翻译)；我们将在下一节用于3D数学的时候好好看一下它们。现在，我们仅仅是将它们的值设为0和1。着色器然后做了一些数学计算来将我们的屏幕空间点`positions`从屏幕空间(-1到1)映射到纹理空间(0到1)并将结果赋值给顶点的`texcoord`。

![](http://duriansoftware.com/joe/media/gl2-vertex-shader-01.png)

跟C很相似，GLSL着色器从main函数开始执行，在GLSL中main函数不接受参数并返回void。GLSL借用了C的预处理关键字用于它自己的指令。`#version`指令表明下面源代码的GLSL版本；我们的`#version`声明了我们使用GLSL版本1.10(GLSL版本跟OpenGL版本绑定得很紧；1.10是对应于OpenGL 2.0)。GLSL去掉了指针和大多数的C中的各种大小的数值类型，只保留了常用的bool,int和float类型，但是它添加了一系列的向量和矩阵类型，长度最多为4个单元大小。这里你看到的vec2和vec4类型分别是两元素和四元素的float向量。类型名也可以作为这些类型的构造函数使用；你可以使用单值构造一个向量，构成的向量的每个元素都将是这个值，或者从向量和单值的混合构造，它们会绑到一起成为一个更大的向量。GLSL的数学操作和一些内置函数是定义在这些向量类型之上的，可以执行元素级的计算。除了数值类型，GLSL还提供特殊的sampler数据类型用于纹理取样，在下面片元着色器中我们将会看到。这些基本类型可以集合成数组和用户自定义的struct类型。

顶点着色器使用GLSL程序中特殊定义的全局变量和绘图管线环境进行通信。它的输入来自于`uniform`变量以及`attribute`变量，分别提供状态值和顶点数组的每个顶点属性。着色器将它的每个顶点输出赋值到`varying`变量。GLSL预定义了一些`varying`变量来接收绘图管线中使用的特殊的输出，包括这里我们使用的gl_Position变量。

## 片元着色器

现在让我们看一下片元着色器源代码，在[hello-gl.f.glsl](http://github.com/jckarter/hello-gl/blob/master/hello-gl.f.glsl)中：

	#version 110
	
	uniform float fade_factor;
	uniform sampler2D textures[2];
	
	varying vec2 texcoord;
	
	void main()
	{
	    gl_FragColor = mix(
	        texture2D(textures[0], texcoord),
	        texture2D(textures[1], texcoord),
	        fade_factor
	    );
	}

在片元着色器中，有些轻微的变化。`varying`变量成了这里的输入：每个片元着色器中的varying变量是跟顶点着色器中的同名变量链接在一起的，并且对这个变量，每个片元着色器调用都接收到一个光栅化的顶点着色器的输出。片元着色器也给出了一系列不同的gl_*预定义变量。gl_FragColor是其中最重要的，着色器将会给它一个vec4的RGBA颜色值。片元着色器可以访问到跟顶点着色器同样的`uniform`系列，但是不能访问到`attribute`变量。

![](http://duriansoftware.com/joe/media/gl2-fragment-shader-01.png)

我们的片元着色器使用GLSL内置的texture2D函数来对两个纹理从texcoord的uniform状态进行取样。然后它调用内置的mix函数基于当前的fade_factor值对两个纹理值进行组合：0会输出只有第一个纹理的取样，1只会输出第二个纹理的取样，而中间的值会给出两者的一个混色。

既然我们已经察看了GLSL着色器代码，让我们回到C并加载着色器到OpenGL。

## 存储我们的着色器对象

	static struct {
	    /* ... fields for buffer and texture objects */
	    GLuint vertex_shader, fragment_shader, program;
	    
	    struct {
	        GLint fade_factor;
	        GLint textures[2];
	    } uniforms;
	
	    struct {
	        GLint position;
	    } attributes;
	
	    GLfloat fade_factor;
	} g_resources;
	
首先，让我们添加一些域到我们的g_resources结构体中，存储我们的着色器对象名字和创建后的程序对象。类似缓冲和纹理对象，着色器和程序对象也是用GLuint句柄命名。我们还添加了一些域来存放整型变量，我们需要在我们的着色器的uniform和attribute变量引用它们。最后，我们添加了一个域来存浮点数值，我们将在每一帧把fade_factor赋值给它。

## 编译着色器对象

	static GLuint make_shader(GLenum type, const char *filename)
	{
	    GLint length;
	    GLchar *source = file_contents(filename, &length);
	    GLuint shader;
	    GLint shader_ok;
	
	    if (!source)
	        return 0;
	        
OpenGL从GLSL源代码编译着色器对象并保存生成的GPU机器码。没有一个标准的方式来将GLSL程序预编译成一个二进制--你必须每次都从源代码编译着色器。这里我们在一个单独的文件中写着色器代码，这样每次我们改变着色器代码时就不用重编译我们的C代码。

	shader = glCreateShader(type);
	    glShaderSource(shader, 1, (const GLchar**)&source, &length);
	    free(source);
	    glCompileShader(shader);


着色器和程序对象脱离了缓冲和纹理所使用的那套[glGen和glBind协议](http://www.zenlife.tk/an-intro-to-modern-opengl-2-1.md)。不像缓冲和纹理函数，操作着色器和程序的函数直接使用对象的整数名作为参数。对象不需要绑定到任何目标。这里，我们对过调用`glCreateShader`创建一个着色器对象，着色器参数可以是GL_VERTEX_SHADER或者GL_FRAGMENT_SHADER。然后我们提供一个源代码的字符串指针给glShaderSource，并告诉OpenGL去使用glCompileShader编译着色器。这一步跟C的编译处理过程很类型；编译的着色器对象也是类型一个.o或者.obj文件。正如C项目中一样，任意多的顶点着色器和片元着色器可以被链接到一起形成一个工作的程序，每个着色器对象引用到其它同类型着色器对象中定义的函数，只要被引用函数全部可以被解析并且顶点着色器和片元着色器的main函数都提供了。

	    glGetShaderiv(shader, GL_COMPILE_STATUS, &shader_ok);
	    if (!shader_ok) {
	        fprintf(stderr, "Failed to compile %s:\n", filename);
	        show_info_log(shader, glGetShaderiv, glGetShaderInfoLog);
	        glDeleteShader(shader);
	        return 0;
	    }
	    return shader;
	}

同样正如C程序，一个着色器的代码块可能会由于语法错误，引用不存在的函数，或者类型不匹配而链接失败。OpenGL对每个着色器对象维护一个由GLSL编译器发出的错误或警告信息记录。在编译着色器之后，我们需要使用glGetShaderiv检查它的GL_COMPILE_STATUS。如果编译失败了，我们使用show_info_log函数显示信息记录并放弃。下面是show_info_log函数：

	static void show_info_log(
	    GLuint object,
	    PFNGLGETSHADERIVPROC glGet__iv,
	    PFNGLGETSHADERINFOLOGPROC glGet__InfoLog
	)
	{
	    GLint log_length;
	    char *log;
	
	    glGet__iv(object, GL_INFO_LOG_LENGTH, &log_length);
	    log = malloc(log_length);
	    glGet__InfoLog(object, log_length, NULL, log);
	    fprintf(stderr, "%s", log);
	    free(log);
	}
	
我们将glGetShaderiv和glGetShaderInfoLog函数作为参数传给show_info_log，这样我们可以在后面对程序对象重用函数(那些PFNGL*函数指针名是由GLEW提供的)。我们使用GL_INFO_LOG_LENGTH参数调用glGetShaderiv来得到信息记录的长度，分配缓冲来存放它，并使用glGetShaderInfoLog来得到它的内容。

## 链接程序对象

	static GLuint make_program(GLuint vertex_shader, GLuint fragment_shader)
	{
	    GLint program_ok;
	
	    GLuint program = glCreateProgram();
	    glAttachShader(program, vertex_shader);
	    glAttachShader(program, fragment_shader);
	    glLinkProgram(program);
	    
如果着色器对象是GLSL编译过程的对象文件，那么程序对象在完成时是可执行的。我们使用`glCreateProgram`创建一个程序对象，使用`glAttachShader`附上着色器对象跟它进行链接，最后使用`glLinkProgram`调用链接过程。

	    glGetProgramiv(program, GL_LINK_STATUS, &program_ok);
	    if (!program_ok) {
	        fprintf(stderr, "Failed to link shader program:\n");
	        show_info_log(program, glGetProgramiv, glGetProgramInfoLog);
	        glDeleteProgram(program);
	        return 0;
	    }
	    return program;
	}
	
当然，链接也可能会失败，由于被引用函数未定义，缺少main函数，片元着色器使用了非顶点着色器提供的`varying`输入，以及其它一些类似C程序链接失败的原因。我们检查程序的GL_LINK_STATUS并将它的日志信息使用show_info_log导出，这次使用用于program的glGetProgramiv和glGetProgramInfoLog函数。

现在我们将`make_resources`用来编译和链接我们着色器的最后一部分填上：

	static int make_resources(void)
	{
	    /* make buffers and textures ... */
	    g_resources.vertex_shader = make_shader(
	        GL_VERTEX_SHADER,
	        "hello-gl.v.glsl"
	    );
	    if (g_resources.vertex_shader == 0)
	        return 0;
	
	    g_resources.fragment_shader = make_shader(
	        GL_FRAGMENT_SHADER,
	        "hello-gl.f.glsl"
	    );
	    if (g_resources.fragment_shader == 0)
	        return 0;
	
	    g_resources.program = make_program(
	        g_resources.vertex_shader,
	        g_resources.fragment_shader
	    );
	    if (g_resources.program == 0)
	        return 0;

## 查找着色器变量位置

	    g_resources.uniforms.fade_factor
	        = glGetUniformLocation(g_resources.program, "fade_factor");
	    g_resources.uniforms.textures[0]
	        = glGetUniformLocation(g_resources.program, "textures[0]");
	    g_resources.uniforms.textures[1]
	        = glGetUniformLocation(g_resources.program, "textures[1]");
	
	    g_resources.attributes.position
	        = glGetAttribLocation(g_resources.program, "position");
	
	    return 1;
	}	        
	
GLSL链接器将一个GLint位置赋值到每个uniform变量和顶点的attribute。uniforms或者attributes的结构体和数组会被继续分解，每个域都会对它的位置赋值。当我们使用程序进行渲染时，将变量赋值到uniform变量以及映射顶点数组的属性，我们将需要使用这些整数位置。这里，我们使用函数`glGetUniformLocation`和`glGetAttribLocation`来查找这些位置，以字符串形式给它们变量名，结构体域名，或者数组元素名字。我们然后在我们程序的g_resource结构体中记录这些位置。程序链接在一起，并且记录中有了uniform和attribute位置，我们可以准备好了使用程序进行渲染。

## 下次，渲染

我知道我在吊你胃口，最后部分还没完成，还没有一个完整的可以运行的程序。我将在在下次，也就是本章最后一部分，修复它，到时我会写代码让绘图管线运作起来渲染我们的场景。

[<< 上一节](an-intro-to-modern-opengl-2-1.md) | [目录](an-intro-to-modern-opengl-0.md) | [下一节 >>](an-intro-to-modern-opengl-2-3.md)
