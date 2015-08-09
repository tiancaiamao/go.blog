目前比较熟习的是C和Go语言。考虑到工作要用于Java，还是要学习一下。

- 跟C和Go不同，Java中常量是用的final，const只是作为保留字，没有使用
- Java的文件命令必须按类名来，跟Go类似是有些额外的约定的
- 关键字instanceof,extends,implent...C和Go中没有。C++语法我已经忘记干净了。
- 有byte和char，byte是一字节，等同于C中的char。而char是unicode的字符单元，跟Go的rune类似。
- 推荐的数组声明方式是int[] a，也可以像C那样写。C语言中是int a[]，而Go语言是var a []int。
- Java的System.out.println，C的printf，Go的fmt.Println
- Java的String是大写的，Go中是小写，C中是char *。
- Java的局部变量不赋初值就使用会编译报错，C中编译不出错，使用是随机的值，Go中会初始化为空值。
- Java的类变量和实例变量不初始化默认是空值，跟C的全局变量类似。
- Java有函数重载，跟C++类似。Go和C没有。
- 接口的语法跟Go有点类似，不过是interface 接口名 {...}而不是type 接口名 interface{}。
- for-each语法是for(String s : list)，而Go是for s := range list 
