以前从来没想到过，在工作中还会用到非递归遍历的写法，那玩意狗都嫌弃。结果发现，还是有使用场景的。

有些用户的 SQL 写得特别奇葩，一条 SQL 几千个 OR .. AND 这种条件连接，还有些是 IFNULL 上千条的，这种一般是生成出来的 SQL，而不是用户手写。这种表达式用递归写法搞的时候，很容易搞爆栈。

我们在数据库的表达式相关的代码里面，大量地使用了递归写法，所以有很多地方会遇到问题。有没有什么通用的解法，能够把递归写法转成非递归写法呢？我分析了几处，发现并不能有某个通用改写方式，只能具体情况具体分析。

以[这里](https://github.com/pingcap/tidb/pull/43886/files)为例，它是把 a and b and c ... 这种结构改写到一个数组里面

```
func splitNormalFormItems(onExpr Expression, funcName string) []Expression {
	switch v := onExpr.(type) {
	case *ScalarFunction:
		if v.FuncName.L == funcName {
			var ret []Expression
			for _, arg := range v.GetArgs() {
				ret = append(ret, splitNormalFormItems(arg, funcName)...)
			}
			return ret
		}
	}
	return []Expression{onExpr}
}
```

递归写法中将孩子树的转成 `[]Expression`，再重组追加到最终结果，并返回。

如果改成非递归，我们只需要遍历表达式树结构，遍历过程中将节点到到最终的返回数组。先序遍历的代码结构是：

```
进栈根节点
while(栈不为空)
	当前节点 = 出栈
	处理当前节点
	进栈右孩子
	进栈左孩子
```

所以这个函数用非递归改写之后，就变成了这样子：

```
func splitNormalFormItems(onExpr Expression, funcName string) []Expression {
	ret := make([]Expression, 0, 1)
	stack := make([]Expression, 0, 5)
	stack = append(stack, onExpr)    // 进栈根节点
	for len(stack) > 0 {             // while栈不为空
		curr := stack[len(stack)-1]  //     当前节点 = 出栈
		stack = stack[:len(stack)-1]
		switch v := curr.(type) {
		case *ScalarFunction:
			if v.FuncName.L == funcName {
				args := v.GetArgs()
				for i := len(args) - 1; i >= 0; i-- {
					stack = append(stack, args[i])      // 将右孩子进栈，将左孩子进栈
				}
				continue
			}
		}
		ret = append(ret, curr)      // 处理当前节点
	}
	return ret
}
```

这里的[这个例子](https://github.com/pingcap/tidb/pull/44108/files)就比较复杂了，它并不能简单地通过树的非递归遍历这种方式改写，因为树的结构不确定，这是一个 visitor 模式。

visitor 模式中，每个 ast 树节点都可实现自己的 `Accept()` 函数，这个函数接受 `Visitor` 所为参数，并返回处理后的新的节点。`Accept` 函数中要递归处理自己的孩子节点，调用它们的 `Accept` 函数并传入 visitor。这个是 `BinaryOperatorExpr` 的实现：

```
func (n *BinaryOperationExpr) Accept(v Visitor) (Node, bool) {
	newNode, skipChildren := v.Enter(n)
	if skipChildren {
		return v.Leave(newNode)
	}
	n = newNode.(*BinaryOperationExpr)
	node, ok := n.L.Accept(v)
	if !ok {
		return n, false
	}
	n.L = node.(ExprNode)
	node, ok = n.R.Accept(v)
	if !ok {
		return n, false
	}
	n.R = node.(ExprNode)
	return v.Leave(n)
}
```

如果不用 visitor 模式，我们本来可以这么写代码：

```
func handle(tree Expr) Expr {
      switch raw := tree.(type) {
	      case BinaryOperationExpr:
		       L1 := handle(raw.L)
		       R1 := handle(raw.R)
			   return BinaryOperationExpr{L: L1, R: R1 ...}
		  case BetweenExpr:
			  ...
		  case CaseExpr:
		  case ColumnNameExpr:
		  case CompareSubqueryExpr:
		  case IsNullExpr:
		  case IfNullExpr:
		  ....
	  }
}
```

需要用很大的 switch case 对所有 Expr 类型做断言。由于树的结构不确定了，`Accept()` 的非递归改写就不太好办。

假用函数式编程那边的写法，有一种实现方式是把代码手工做 CPS 变换，通过 CPS 变换之后，所有的递归都了尾递归。
接着，使用 trampoline 这类的技巧，使尾递归不让栈增长。

还是以 `BinaryOperationExpr` 的 `Accept` 为例子，通过 CPS 变换，(忽略错误处理)代码变成：

```
func (n *BinaryOperationExpr) Accept(v Visitor, cc func(Node, bool)) {
	newNode, skipChildren := v.Enter(n)
	if skipChildren {
		cc(newNode, ok)
		return
	}

	n = newNode.(*BinaryOperationExpr)
	n.L.Accept(v, cc1 func(node, ok) {
		n.L = node.(ExprNode)

		n.R.Accept(v, cc2 func(node, ok) {
			n.R = node.(ExprNode)
			cc(v.Leave(n))
			return
		})
	})
}
```

`Accept` 从返回 `(Node,bool)`，变成了接受一个(回调)函数，这个函数会接受 `(Node,bool)` 并执行接下来的处理(这个函数，在 scheme 中术语叫continuation，接受一个执行继续后续的处理的函数，就是 continuation)。


经过 CPS 变换之后，不再有返回 xxx 的概念，而是以参数 xxx 调用连续。在不支持尾递归的语言里面，这样的调用还是会使用栈增长。需要继续使用 trampoline 的技巧变换。

trampoline 很简单，就是：

```
while (next != nil) {
	next = next()
}
```

把接下来要执行的东西，以函数指针之类的形式返回，在 trampoline 的下一次循环去调用。如果我们把 CPS 版本的 Accept 函数包装在 trampoline 里，代码就不能写成 `cc(newNode)`，而应该是 trampoline.next = 包装 (cc, newNode) 成一个可执行的函数。递归 `Accept` 不能写成 `Accept(v, cc)`，而需要写成 trampoline.next = 包装 (v, cc) 成一个可执行的函数...

通过 CPS+trampoline 递归转非递归最终变换出来的代码...简直是没法读了：

```
func (n *BinaryOperationExpr) acceptV2(t *trampoline, v Visitor, cc0 func(Node, bool)) {
	l, ok1 := n.L.(nonRecursive)
	r, ok2 := n.R.(nonRecursive)
	if !ok1 || !ok2 {
		res, succ := n.acceptV1(v)
		t.continueWith(cc0, res, succ)
		return
	}

	newNode, skipChildren := v.Enter(n)
	if skipChildren {
		res, succ := v.Leave(newNode)
		t.continueWith(cc0, res, succ)
		return
	}
	n = newNode.(*BinaryOperationExpr)
	cc1 := func(node Node, ok bool) {
		if !ok {
			t.continueWith(cc0, n, false)
			return
		}
		n.L = node.(ExprNode)

		cc2 := func(node Node, ok bool) {
			if !ok {
				t.continueWith(cc0, n, false)
				return
			}
			n.R = node.(ExprNode)
			ret, succ := v.Leave(n)
			t.continueWith(cc0, ret, succ)
		}

		t.tailcallWith(r, v, cc2)
	}
	t.tailcallWith(l, v, cc1)
}
```
