chibi-scheme 的内存管理，维护着一些受控堆区。所有的 scheme 对象都是在受控堆中分配的，垃圾回收也是局限于此受控堆区。

保守的垃圾回收算法会作用于整个堆区，而使性能受影响。而这里只有 scheme 语言的堆区和 sexp 对象是垃圾回收的，因此嵌入到C中时宿主程序不会受影响。

## 第一部分:堆结构

```
typedef struct sexp_free_list_t *sexp_free_list;
struct sexp_free_list_t {
sexp_uint_t size;
sexp_free_list next;
};

typedef struct sexp_heap_t *sexp_heap;
struct sexp_heap_t {
sexp_uint_t size， max_size;
sexp_free_list free_list;
sexp_heap next;
/* note this must be aligned on a proper heap boundary， */
/* so we can't just use char data[] */
char *data;
};
```

每个 heap 通过 next 字段链接在一起，size 记录当前块的空间大小。当空间不够时，堆的增长方式是每一块是前一块的大小的 2 倍。freelist 是一个带头结点的自由链。

通过上述结构体，堆的实现已经很清晰了，代码比较简单，不做具体分析。可以看gc.c中的 `sexpmakeheap()` 函数。

需要注意的是对齐的存在。对齐会使得后面垃圾回收时的标记清除方便许多，因此使用对齐。

## 第二部分:垃圾回收

默认使用的垃圾回收是采用标记-清除法。函数 `sexp_gc` 会调用 `sexp_mark` 和 `sexp_sweep`。由于有些资源需要析构，所以清除之前还调用了一个 `sexp_finalize()`。这个函数调用 sexp 对象的"析构函数" finalizer 以释放资源，比如对于打开的文件描述符。

`void sexp_mark (sexp ctx， sexp x)` 函数 `sexp_mark` 以 x 作为根，在 ctx 的堆中进行游走，遇到没有标记过的则进行标记，并递归地进行这一标记过程。

`sexp sexp_sweep (sexp ctx， sizet *sumfreedptr)` 函数 `sexp_sweep` 对 ctx 的堆进行扫描，遇到没有标记的则回收空间。其中释放空间就是将此部分空间放到 freelist 中，可能涉及合并操作。垃圾回收时，需要一个根结点作为起点。这个根结点选取的正是上下文对象 ctx。所有的对象都封装在了上下文对象中，因此从上下文对象作为根出发才可以访问到所有的有用的对象，避免其被作为垃圾清除了。值得注意的是，上下文对象本身的存储空间也是在堆中分配的，也就是上下文对象中有个堆成员，而上下文对象的自身存储空间也是在堆中分配的。

这看似一个鸡生蛋蛋生鸡的问题，那么第一个上下文对象是怎么来的呢？这个可以查看文件 sexp.c 中的 `sexp_bootstrapcontext` 函数。

## 第三部分:临时变量保护

假设我想使用两个浮点型的临时变量，使用如下代码:

```C
sexp f1 = sexp_make_flonum(ctx，1.0);
sexp f2 = sexp_make_flonum(ctx，2.0);
```

这样使用会隐含着一个 bug:在创建 f2 时调用链会调用到 `sexp_alloc`，该函数会先调用 `sexp_tryalloc`，如果失败，则调用 `sexp_gc` 之后再尝试。由于 f1 没有加以保护，在 gc 的过程中它可能被当作垃圾清理了，因此这种时候临时变量必须保护起来。

所谓保护，就是使得上下文对象能够跟踪到它，因此上下文对象中有一个 saves 链，作用就是保护这种临时变量的。把临时变量的地址放放到 saves 链中，垃圾回收的 mark 阶段临时变量就会被加上标记，sweep 阶段不会被清除。

```C
struct {
  sexp_heap heap;
  struct sexp_gc_var_t *saves;
　    ...
} context;
```

因为是临时变量，使用完之后又要注意在 ctx 的 saves 链中清除它，否则会永远占据内存空间不被释放。为此 chibi-scheme 定义了一组宏，以方便处理:

```C
struct sexp_gc_var_t {
  sexp *var;
  struct sexp_gc_var_t *next;
};
#define sexp_gc_var(ctx， x， y)                  \
  sexp x = SEXP_VOID;                           \
  struct sexp_gc_var_t y = {NULL， NULL};

#define sexp_gc_preserve(ctx， x， y)     \
  do {                                  \
    sexp_gc_preserve_name(ctx， x， y);   \
    (y).var = &(x);                     \
    (y).next = sexp_context_saves(ctx); \
    sexp_context_saves(ctx) = &(y);     \
  } while (0)

#define sexp_gc_release(ctx， x， y)   (sexp_context_saves(ctx) = y.next)
```

这段代码写得是相当的精妙啊! `sexp_gc_vart` y是放在c函数的栈中的，因此出了函数栈保护就失效了。但正是由于 `sexp_gc_var` 声明的目的也是作为临时变量使用，因此这种处理洽到好处。chibi-scheme还定义了sexpgcvar1到sexpgcvar6的宏，分别是一次声明 1 到 6 个临时变量。

如果想保存永久变量该如何呢？chibi-scheme 提供了类似的 `sexp_preverse_object` 和 `sexp_release_object` 两个宏。实现的机制与上面类似。具体在下一部分讲解。

## 第四部分:上下文结构初窥

```
struct {
      sexp_heap heap;
      struct sexp_gc_var_t *saves;
#if SEXP_USE_GREEN_THREADS
      sexp_sint_t refuel;
      unsigned char* ip;
      struct timeval tval;
#endif
      char tailp， tracep， timeoutp， waitp;
      sexp_uint_t pos， depth， last_fp;
      sexp bc， lambda， stack， env， fv， parent， child，
        globals， dk， params， proc， name， specific， event;
#if SEXP_USE_DL
      sexp dl;
#endif
    } context;
```

由于上下文在 chibi-scheme 中是如此重要的一个结构，这里进行部分分析。更详细的在 eval 中还会讲到。

上下文是垃圾回收算法的标记的根。上下文还是编译器执行一个s表达式所必须的结构。当一个s表达式要执行时，它需要一个上下文，这个上下文里包含了堆，栈，环境，等等。

可以看上面有 heap，stack，env等字段。parent 和 child 字段用于将各个上下文结构联系起来 .bc 的意思是 bytecode，s表达式会被编译成字节码，在虚拟机中执行。

fv 的意思是 free value 自由变量，这和闭包等是 lisp/scheme 语言中的概念。

tailp 字段是用于尾调用优化的，scheme 语言标准要求必须是严格尾递归的。

saves 前面刚讲过，它是为了保护临时变量在 gc 时不被回收掉。

env 环境也是 scheme 中的一个重要概念，环境中，包含了 name 到 value 的绑定，在编译过程中，编译器会通过环境查找变量的真实值。

现在重点看 globals。它的真实类型是一个 vector 对象，大小是 SEXPGNUMGLOBALS。存的是一些全局信息。每一个槽位都有各自的作用。

```
enum sexp_context_globals {
  SEXP_G_TYPES，
  SEXP_G_NUM_TYPES，
  SEXP_G_OOM_ERROR，             /* out of memory exception object */
  ...
  SEXP_G_PRESERVATIVES，
  SEXP_G_NUM_GLOBALS
};
```

其中 SEXPGPRESERVATIVES 槽是一个保护链，放置需要保护的非临时变量的，原理与 saves 链类似。

```C
#define sexp_global(ctx，x)      (sexp_vector_data(sexp_context_globals(ctx))[x])
void sexp_preserve_object(sexp ctx， sexp x) {
  sexp_global(ctx， SEXP_G_PRESERVATIVES) = sexp_cons(ctx， x， sexp_global(ctx， SEXP_G_PRESERVATIVES));
}
```

还记得上一篇的类型对象表，我们当时为了便于理解叫table，这个表正是存于 globals 的 SEXPGTYPES 槽的，即 `sexp_global(ctx， SEXPGTYPES)`。

总之，上下文是一个非常复杂的结构，chibi-scheme 使用这个结构来对s表达式进行求值。

## 小结

首先给出了用于内存管理的堆的结构，这个比较简单，代码没有展开讲。熟悉C语言的写过内存池或自己的内存管理系统的很容易就能理解。

垃圾回收使用的标记清除算法，原理网上有很多，知道原理了代码很容易就能看懂。值得注意的是标记的根是使用的上下文对象，这是一个很巧妙的设计。

不加保护地使用临时变量可能遇到的陷阱。要使用 API 中提供的 `sexp_gc_var` 1 至 n，`sexp_gc_preverse` 加以保护，使用完毕之后注意用 `sexp_gc_release` 释放。

上下文在整个 chibi-scheme 中的地位非常重要，这里进行了一些说明。后面具体讲 eval 时还会提到。看似与本篇的题目无关，但无论是作为垃圾回收的根，还是 globals 的保存链都有所涉及。并且给出中下文对象的结构也是为后面几篇做个铺垫。

这些都是基础，只有对类型系统和内存管理游刃有余之后，读后面的代码才不至于不停的回头查看前面的函数定义。
