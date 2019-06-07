主要文件在 lisp.h 和 eval.c

symbol 对象包含4个域:

name 符号名

value 值

function 函数

plist 属性链

隐含一个 next 域指针，指向下一个 symbol

有个全局的 obarray，其实就是个 vector 类型的 lisp 对象。

---

符号名通过 hash 确定在 obarray 中的哪一个桶，然后根据 next 域出去找到最终的 symbol 对象

```
struct specbinding
{
Lisp_Object symbol, old_value;
specbinding_func func;
Lisp_Object unused;      /* Dividing by 16 is faster than by 12 */
};
```

全局有个 specpdl 指向 specbinding 的数组，specpdlptr 指向数组的当前位置

相当于一个栈。通过这个结构体做绑定的。

每进入一个新的作用域，比如 `(let (x) ...)`，将符号 x 的旧值放在 specpdlptr 中存起来

x 的新值替换 x 的 symbol对象中原来的 value。

当退出作用域的时候用 specpdlptr 中的值恢复 x 的原值

elisp 中没有环境的概念，就通过全局的 obarray 寻找符号的值，通过 specbinding 保存符号在各调用栈上的值

其实本质上就相当于 elisp 中，仅有一个全局的 env，就是 obarray。

然后每个符号都有一个值的栈。进放一个作用域就进栈一个值，出作用域时这个值出栈，符号始终绑定到栈顶的值

没有闭包。闭包=代码+环境。这里环境不成立了

emacs 的源代码比较乱，文档对于 internals 的解释也不好。

对照 xemacs internals 看比较容易看懂。
