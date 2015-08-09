C语言中，返回在栈上分配的结构体是不安全的，因为函数的栈空间在返回之后就会被清除掉：

    struct Ret* f() {
        Ret ret;
        return &ret;
    }

如果想返回一个结构体，那么就需要调用malloc分配堆空间：

    struct Ret* f() {
        Ret *ret = malloc(sizeof(struct Ret));
        return ret;
    }

但是如果函数f本身是一个非常简单的函数，那么malloc分配一个返回值的代价就有点大。所以C语言常用的约定是参数既是输入也是输出，由调用函数分配空间：

    void callee(struct Ret* ret) {
        // do something with ret
        return;
    }
    void caller() {
        Ret ret;
        callee(&ret);
    }

有没有更屌的方式呢？回调！如果做一个CPS变换，传一个回调函数作为参数，那么被调函数栈空间仍然是有用的：

    typedef void (Callback)(struct Ret *ret);
    void callee(Callback callback) {
        struct Ret ret;
        callback(&ret);
    }
    void dummy(struct Ret *ret) {
        // process ret here, it's valid
    }
    caller(dummy);

这时的栈布局中：
caller栈----
callee栈---
Ret
dummy栈---

有点奇技淫巧的感觉...
