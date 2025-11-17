C语言以 '\0' 结尾的字符串的设计非常糟糕，所以有不少三方的库提出不同的设计。关于这个话题，我看了一下几个 string 库的设计，总结一下。

首先是 [bstring](https://raw.githubusercontent.com/websnarf/bstrlib/master/bstrlib.txt) 这个库，文档写得非常好。它列的 C 的字符串的毛病可以复述一下：

1. 使用 '\0' 结尾的表示，会导致 string 长度是 O(n) 而不是 O(1)
2. 这个设计导致对于字符 '\0' 需要特殊的解释和处理
3. gets() 暴露了缓冲区溢出的风险，增加了应用的安全漏洞
4. strtok() 在 parse 的时候会修改字符串，这个方法不是可重入的，也不线程安全
5. fgets() 是以 '\n' 结尾，如果遇到 '\0' 其语义也要特殊考虑
6. 没有好的内存管理，像 strcpy strcat 和 sprintf 等都很容易缓冲区溢出
7. strncpy() 在有些情况下不会保证以 '\0' 结尾
8. 传 NULL 到 C 的字符串库会造成空指针访问
9. overlapping 或者参数是自身引用时，大多的 C 标准库函数都是未定义行为
10. 大多数 C 库都是接受 integer 参数却没检查 range，出错后很容易导致后续难以定位并完全不可预知的行为

bstring 里面定义这样的结构体，而 bstring 类型定义就是这个结构体的指针。

```
struct tagbstring {
	int mlen;
	int slen;
	unsigned char * data;
};
```

slen 是真实的字符串的长度，不算 '\0'。mlen 是这块 data 的内存大小。它用到了一个技巧是，**让 mlen 的长度大于 slen，并且将最后字节填充一个 '\0'，这样可以兼容 C 的字符串设计**。这个技巧在后面几乎所有库中都被用到了。

bstring 库可能比较老？文档比较好，其它的就没有太多新意了。关于值和引用，它没有提所有权的概念，只定义了一个 `const_bstring` 的类型别名，如果只读不会被修改，则用 `const_bstring` 否则默认用 `bstring`:

```
typedef struct tagbstring * bstring;
typedef const struct tagbstring * const_bstring;
```

然后是[这个库](https://github.com/maxim2266/str)，不怎么知名。但是它里面有一个很有意思的点，它在 size 信息里面，**用了一位的 bit 来存储这个 string 是引用，还是 owner**。

```
typedef struct
{
	const char* ptr;
	size_t info;
} str;

#define _ref_info(n)	((n) << 1)
#define _owner_info(n)	(_ref_info(n) | 1)
```

因为字符串跟内存管理强耦合着，一个很需要注意的问题就是"谁"负责释放内存。区分引用和 own 在这种场景下，非常有意义。如果是引用，就不要去释放内存，而如果是 owner 则可以。
这个库比较小，靓点就在于这个区别引用的 owner 的方式吧，把这个信息藏到了 size 字段里面。


接下来还有 [sds](https://github.com/antirez/sds) 这个库。这个库是 redis 作者常年使用，最早是在 redis，后面独立出来放到单独的 repo 里面了。由于 redis 的知名，这个库还是很知名的。
它的设计点里面，**很巧妙的一个地方是将 header 放在了返回指针的头部，而不像常规的做法那样用一个结构体表示**：

```
+--------+-------------------------------+-----------+
| Header | Binary safe C alike string... | Null term |
+--------+-------------------------------+-----------+
         |
         `-> Pointer returned to the user.
```

这个库的设计上，最重要的就是强调它跟 C 的兼容性。如果不考虑前面 header 的信息，它就是跟 C 的表示一模一样的。所有可以用 C 标准库的地方，都可以传 sds 的字符串。
这个 header 的表示形式，其实我之前在 [fat pointer](/fat-pointer.md) 里面已经见识过了。

另外，sds 还有一个不同于其它字符串库的设计点，它是**把数据紧接着结构体分配的，而不是像其它实现中定义为一个指针**:

```
struct sdshdr {
    int len;
    int free;
    char buf[];
};
```

这样做的好处是访问 len 和实际的数据信息，其实上都是相邻的位置，对硬件的缓存友好性。

sds 的 API 设计里面，有一个 range 函数是取子字符串的，它可以支持负数索引，-1 表示字符串结尾，-2 是倒数第二个... 这个点我觉得还挺好用的。


最后一个是 [Modern C and What We Can Learn From It](https://www.youtube.com/watch?v=QpAhX-gsHMs)，youtube 上面的一个视频，里面有一小节是提到关于 string 处理的。里面有个设计点是：**严格区分 owning 和 borrow 的 string**。
还有就是把内存分配的事情，从字符串处理里面独立出去。

```
typedef struct str
{
	char *data;
	isize_t size;
}
typedef struct str_buf
{
	char *data;
	isize_t size;
	isize_t capacity;
	allocator_cb allocator;
}
```

相对于前面 str 那个用一个 bit 位来区分，这里面彻底地用了两个不同的数据结构。如果是 owning 的，则对应到 `str_buf`，而如果是 borrow 的，是一个只读对象，对应到 `str`。

在这个视频的 [ppt](https://accu.org/conf-docs/PDFs_2021/luca_sass_modern_c_and_what_we_can_learn_from_it.pdf) 里面，有提到一个 `str_pop_first_split` 函数，并给出了用法：

```
str str_pop_first_split(str *str, str split_by);
str date = cstr("2021/03/12);
str year = str_pop_first_split(&data, cstr("/"));
str month = str_pop_first_split(&data, cstr("/"));
str day = str_pop_first_split(&data, cstr("/"));
```

我发现这个东西本质上就是 Go 语言里面的 [Cut 函数](https://pkg.go.dev/strings#Cut)。russ cox 大佬在 Go1.18 里面加了这个[Cut 函数](https://github.com/golang/go/issues/46336)，因为这个 pattern 非常常见，重复了非常多次，一开始 Go 里面没有提供，后来又加了进来。跟这里的 `str_pop_first_split` 真是惊人的巧合(或者叫英雄所见略同)。


我把所有的觉得设计上好的点再列出来：

* 放弃 '\0' 的 C 字符串表示，新的表示中存储 len 的数据，再自定义一套字符串库
* 让分配的数据空间大于字符串的实际长度，并且将最后字节填充一个 '\0'，这样可以兼容 C 的字符串设计
* 严格区分 owning 还是 borrowing，可以定义成不同的结构，这样可以有利于内存管理
* 数据直接在结构体中分配，而不是另外的指针，有利于缓存友好性
* 子字段串的 API 支持负数作为从后向前索引


最后给一组 API ... 至于实现，读者作为练习嘛~

```
typedef struct _str {
  const char *str;
  unsigned int len;
} str;

struct _strBuf {
  int cap;
  int len;
  char data[];
};

typedef struct _strBuf* strBuf;

str cstr(const char* str);
str toStr(strBuf s);
int strLen(str s);
int strStr(str haystack, str needle);
int strChr(str s, char c);
int strCmp(str s, str s0);
str strSub(str s, int start, int end);
str strCut(str *s, str split);

char* toCStr(strBuf s);
strBuf fromCStr(const char *s);
strBuf fromBlk(const void *ptr, int len);
strBuf strNew(int cap);
void strFree(strBuf buf);
strBuf strDup(str s);
strBuf strCpy(strBuf to, str from);
strBuf strCat(strBuf to, str from);
strBuf strAppendChar(strBuf to, char c);
...
```
