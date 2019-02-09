JVM 的 bytecode 指令集特别非常有规律，很容易记下来。

## 分类

JVM 是一个基于栈的虚拟机，所以 bytecode 指令集设计也是围绕着栈虚拟机相关的的操作。

首先是对指令集进行一下分类。设计一个指令集，肯定各类不同指令会有不同目的的用途，比如：

栈操作 控制指令 运算指令 对象操作 类型转换 常量　函数调用

其中运算指令，就有算术运算，逻辑运算，位运算，大小比较等。Java 是面向对象的语言，对象操作相关的指令也是必须的，另外，还一些围绕数组相关操作的指令。

## 记关键词

分类完毕之后，接下来是记忆关键词。

栈操作： push pop load store dup swap

控制指令： return ret goto if_cmp if nop jsr lookupswitch tableswitch throw

数组操作： newarray arraylength load store multianewarray

算术运算： add sub mul div rem neg

逻辑运算： and or

位运算： shl shr xor

比较运算：　cmp

类型转换的：　checkcast x2y wide

对象操作：new getfield putfield getstatic putstatic instanceof

函数调用：invokedynamic invokeinterface invokespecial invokestatic invokevirtual

常量： const ldc 

## 找规律

1. 类型规律

不少指令是加了一个 a 开头的。比如load 不是 load，而是 aload，从局部变量中 load 一个值到栈上。store 也是 astore，将栈上的值存储到局部变量。还有 areturn。

类型变换的，都是 x2y 形式，比如 double 转 float，就是 d2f。int 转 char，就是 i2c... 只要把类型记下来，专换操作非常好记。类型有 byte short int float double char。

不同类型操作以各类符号开头。比如 load 就是 fload dload 分别用于 float 和 double 类型。布尔是 b 开头，array 是 a 开头，所以很容易想到，baload 是从 array 里面 load 一个 bool。int 是以 i 开头，相应的指令 istore iload ior iand irem 等等。

2. 缩写规律

有些指令可以把操作数跟指令连写，简化成一条指令。形式都是 `xx_<n>`。比如 `aload_1` `dload_2` `iconst_i` 这样子。

函数调用的指令都是以 invoke 开头，不同的调用方法对应到不同的指令。类的静态方法是 invokestatic，接口调用是 invokeinterface。实例方法是 invokespecial，它也处理基类构造，实例初始化。

## 抓重点

对于最常用的，优先记忆。像栈操作的，都很频繁。然后是控制指令，if goto return 这种。然后是 invoke。

剩下就是边边角角的特殊指令了，用的时候查也没关系，像 lookupswitch，monitorenter

最后是[指令集手册](https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-6.html#jvms-6.5)，对照着看。


