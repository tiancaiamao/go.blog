刚刚给shen-go打上了v0.2版。从v0.1到v0.2主要做的是一些性能优化相关的事情，分享一些数据。

最早的时候我实现了一个klambda解释器，那时跑shen的测试需要165s完成。

在发布v0.1版的时候，实现了编译到bytecode，跑测试时间大概是25s的样子，从纯的解释器到未优化过的bytecode，大概是快了6倍多。

接着就是v0.1到v0.2版做的事情，[前面有提到过](shen-go-optimize.md)：

vm的大幅改进，主要是将环境拆成堆上和栈上两部分，尽量地避免在堆上分配对象。加上vm那边一些细节上的优化，这块做了之后时间缩短到了16s。

对hash函数做了 peephole optimizations，时间缩短到15.3s。

接着对variable? integer? 等几个函数做 peephole optimizations 之后，时间缩短到13.9s。

调整了 symbol 的表示，不再是string而直接记一个offset，比较就可以不用string比较。做了这个之后时间是12.5s。

优化了函数调用，从map实现改到了直接数组offset获取，最终是11.6s。

let的优化没做，peephole optimization还可以再彻底一些，感觉得如果做到极致，估计能就刚进10s的样子。

threaded code 和 fixnum tagging 我尝试了一下，在Go语言里面都做不了。如果是换成 C 语言写vm，可以做这些优化，并且内存管理也做的更激进一些，我推测bytecode方式的极限可能在 7-8s。

顺便分享一些 shen 在其它语言下实现的数据。

shen-c 是完全基于解释器实现的，太挫了就不想说它了。

shen-scheme 在 chibi-schem 下的实现，大概是22s的级别。这也是一个字节码实现，还是 C 语言做的，只比我 bytecode 未优化的时候略快一点，也算是挺挫的了。无节操的推测一下可能是scheme的异常处理机制拖累了它。

shen-elisp 是编译到 emacs lisp 的 bytecode，速度大概在30秒刚出头。elisp 不算一门正儿八经的编程语言啦。

shen-jvm 是直接编译到 JVM 的，速度大概在 4.几秒的样子。

shen-cl 官方维护的版本，通过 common lisp 编译到 native 的实现，速度大概也是同样量级，4.5s。比 shen-jvm 慢一点点。

shen-chez，这就是王垠说的世界上最牛的编译器，1.6s！还真是一个秒天秒地的版本，我算是领教了。

F#看论坛上给的结果，好像都是10几秒的版本吧，Truffle 好像更慢一点。我没具体去测。不过如果编译到某个平台的虚拟机，比如.Net或者Graal，这结果还真是挺挫的。

结论就是，bytecode大概就10-20s的量级了，做得非常有节操会进10s内。而JVM虚拟机这种带了JIT或者common lisp到native的大概就4s的级别。最牛B的chez吊打其它。

shen-go 在基于 bytecode 实现里面已经最快的了。基本做不下去。弃坑弃坑

供参考，以上。

(最后，数据都有时效性，只是当下的比较，说不定哪个升级下变牛逼了，就不准确了。

所以嘛，想知道准确的性能测试结果，这东西最好自己做)
