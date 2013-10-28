# 在osx上编译planeshift

[planeshift](http://www.planeshift.it)是一个开源的3d网游。

在我的mac上编译这个东东颇费了一番功夫。照着官方的编译说明弄有些问题，google了好久。比较有用的是[这里](http://linuxaged.github.io/2013/06/11/compile-planeshift-on-osx/)

大概是gcc4.2.1以后，osx用clang替代了gcc作为默认的编译器，但是这个编译器有bug，无法编译varargs的虚函数。所以需要另外安装一个gcc编译器。

然后遇到另一个问题是，在链接过程会出现duplicate symbol 的错误，这个原因是fparser中的bug，svn源中使用的是4.5版本。我自己去下了个fparser 4.5.1，这个版本是修复了这个bug的。然后替换掉planeshift中的src/tools/fparse，修改一下编译脚本就ok了。