scheme 模块笔记

本来想写一篇模块的实现的。结果研究了许久，发现这个东西很难，不好实现。于是就改成写一篇笔记，讲实现模块困难在哪里吧。

卫生

common lisp VS scheme
package namespace vs module

目标：
* separate compilation
* 暴露接口，隐藏实现细节
* 分阶段编译

(let ()
  (define a ...)
  (define b ...)
  (make-module a b ...))


(package p
         (export s)
         (define-syntax s
               ...))
               
宏并不是 first class 的。它不是一个值。

工作在宏展开时期的。所以模块和宏必然产生关联。

lex-env := id -> location
store := location -> value
mod-env := id -> mod
模块环境是 identifier 到 mod 的映射
load-env := 

load 环境是记录哪些模块 load 了，以及它所在的 toplevel 环境


从模块的词法环境，获取 location
然后由 location 在 store 找到值

