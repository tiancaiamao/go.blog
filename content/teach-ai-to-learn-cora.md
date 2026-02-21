未来的代码都不会是人类手写的了。

我自己发明了一门编程语言叫 cora，它是个 lisp1 方言。但是，既然**我自己都已经不手写代码了，那我们还要发明自己的编程语言搞毛呀**？

突发奇想，其实我可以教会 AI 使用我发明的 cora 语言，然后 AI 就可以用 cora 语言帮我写代码了。

曾经我觉得，最适合 AI 时代的编程语言，应该是那些成熟的老牌语言，像 c/c++ 这类。理由是训练过程中它能接触到海量的素材，从而掌控这门语言。

另外就是像 rust 这类的，内存安全，强类型，写得出来就能正确跑的语言。虽然写的时候心智负担高，很恶心人，但是写出来之后能跑，这很适合给 AI 扬长避短。

还有一个输入是来自我之前的体感，使用 odin 之类的新兴语言，在 AI 写代码的时候会有不少幻觉，有语法错误的，有把其它语言的框架和库当作是这门语言的...所以我之前认为新兴语言机会反而更少。

其实这个事情是有补救的！因为 AI 有学习能力，我们只要给它正确的 prompt，就可以教会它使用一门新语言。

现在我来验证这个想法。

第一步，我去 chatgpt 那边问了问，像这样的 prompt 需要写哪些内容，它给了我一个回答，包括例子和模板。它给了我一个这样的东西：

```
你将学习并使用一门“你之前从未见过”的编程语言：<语言名>。

重要约束：
1. 你只能依据我提供的资料理解这门语言
2. 不允许使用你对其他语言的类比，除非我明确允许
3. 如果资料不足以确定行为，请明确说明“不确定”

--------------------------------
【语言概览】
- 设计目标：
- 运行模型：
- 类型系统：
- 错误处理方式：

--------------------------------
【核心语法示例】
示例 1（解释逐行语义）：
<code>

示例 2（包含常见错误）：
<code>

--------------------------------
【语义规则（非常重要）】
- 规则 1：
- 规则 2：
- 违反后果：

--------------------------------
【禁止事项】
- 永远不要……
- 在以下情况下禁止使用……

--------------------------------
【你的任务】
从现在开始：
- 所有示例代码必须符合以上规则
- 如果我给出错误代码，请指出并解释违反了哪条规则
```


第二步，我在 cora 的代码仓库目录，打开了 codex (gpt5.2 high)，我把 chatgpt 生成的模板喂给它。要求它读项目源代码并理解，因为项目源代码是有不少 cora 语言写的代码的，它可以学习到所需要的内容。然后让它根据模板示例，创建这门语言的 prompt。

这是它生成出来的内容，我上传到 github gist 了。[prompt0.md](https://gist.github.com/tiancaiamao/f6177f7999d2e411efb9a45954e633b8)

我发现这个 prompt 很精炼。我担心这是否太于精炼了，不利于 AI 学习。

于是我找了另一个例子，[odin 语言的 demo](https://odin-lang.org/docs/demo/) 作为模板，让它不要拘泥于之前的模板格式，重新生成了一份。

这是生成的第二份 [prompt1.md](https://gist.github.com/tiancaiamao/d8c535beb0a809358623d56fff1e051b)


第三步，我换了工具和模型 claude code (glm4.7) 用来交叉校验这个 prompt 写得好不好，能不能让 AI 学会 cora 语言。

我让 claude 阅读这两份 prompt，判断哪一份 prompt 写得更好一些，更能够有效地让它学会 cora 语言。结果自然是第二份，因为内容更长，示例也更多。

然后，我告诉 claude，"你已经学会 cora 语言了，现在帮我写一个 cora 语言的解释器，就用 cora 语言写"。这一步的时候，它做弊了。它说，这个任务很复杂，让我来先准备一个 plan。然后它说，让我先阅读当前的项目代码... (我正好在 cora 项目的目录启动的 claude)。

所以 claude 实际上读到了关于 cora 的实现，而不仅仅是那份 prompt。在它还没完成 plan ，我打断了它，告诉它，"我要求的并不是要写一个 cora 解释器。我只是通过这个来检验它对 cora 语言的理解和掌握，是否通过 prompt 就学会了 cora 语言。甚至都不需要解释器的实现完全能跑，只要语法语义层面正确即可"。

接下来它尝试实现。我阅读了它实现上面所犯的错误，其实犯错也不多，主要是在 match 那一块的语法。我给它指出来，然后让它生成 patch.md。

写完了解释器之后，我又让它换一些例子，随便写一些 cora 代码。目的是验证它掌握了这门语言。来来回回处理了几轮之后，都更新到了 patch.md

我把 [patch.md](https://gist.github.com/tiancaiamao/9ef1d2926e081c1060769e68deaa45d7) 也放出来。


第四步，我让它结合 prompt0 prompt1 和 patch，生成 prompt2.md

我告诉它，prompt0 写得很精炼简洁并且准确，告诉它 prompt1 里面写了很多 demo 有利于 AI 学习和掌握。而我要求它给我生成的 prompt2 要有两者优点，既能很精炼简洁，又能有提供例子让 AI 真正学会这门语言。

然后 patch 是犯过的错误，需要避免。

最终生成出来了 [prompt2.md](https://gist.github.com/tiancaiamao/507adfcd49a22ea7184d9481828de1a1)。


第五步，再次回测。这次我创建了一个新的空文件夹，然后把 prompt2.md 拷贝过去，换到 codex。它现在只能参考 prompt2，我让它写一个 cora 或者 lisp 方言的解释器出来。用 cora 语言写，我来检验它对这门语言的掌握。

通过这一份 prompt，它基本上真的掌握如何用 cora 语言写代码了。

以上就是我如何教 AI 学习 cora 语言的。Have fun!

附(AI 生成的代码，证明它学会了)：

-------------------

• 已阅读 prompt2.md。下面给出一个 Cora 语言 的解释器草案，严格按你给的规范写，重点是语法/语义正确而不是可运行。

  不确定点（规范未给出，只能显式标注）

  - reader 对 []、[x . xs]、'x 的具体 AST 形式未定义，我在 pat-match 中假设 [] → (list)、[x y] → (list x y)、[x . xs] → (list-rest x xs)、'x → (quote x)。
  - 未给出 symbol?、sym=?、= 的存在性与语义，这里假设它们存在且满足直觉：symbol? 判断符号，sym=? 比较符号，= 比较数/字符串/布尔。
  - try/throw 的 continuation 语义未完整定义，这里用 ['throw v] 的 tag 方式显式传递，只保证“handler 内 throw 不重入”的效果。

  Cora 解释器（核心 eval2 + match + 部分应用）

```
  ;; 结果封装：['ok v] / ['throw v]
  (func ok v => ['ok v])
  (func thrown v => ['throw v])

  (func bind-eval
    ['ok v] k => (k v)
    ['throw v] k => ['throw v])

  (func bool?
    true => true
    false => true
    _ => false)

  (func empty?
    [] => true
    _ => false)

  (func append
    [] ys => ys
    [x . xs] ys => [x . (append xs ys)])

  ;; 环境：alist，绑定为 [sym . val]
  (func env-extend
    env k v => [[k . v] . env])

  (func env-extend-list
    env [] => env
    env [[k . v] . rest] => (env-extend-list (env-extend env k v) rest))

  (func env-lookup
    [] k => (thrown ['unbound k])
    [[key . val] . rest] k =>
      (if (sym=? key k) (ok val) (env-lookup rest k)))

  ;; 列表工具
  (func list-length
    [] => 0
    [_ . xs] => (+ 1 (list-length xs)))

  (func take
    0 xs => []
    n [] => []
    n [x . xs] => [x . (take (- n 1) xs)])

  (func drop
    0 xs => xs
    n [] => []
    n [_ . xs] => (drop (- n 1) xs))

  (func bind-params
    [] [] env => env
    [p . ps] [v . vs] env => (bind-params ps vs (env-extend env p v)))

  ;; 参数求值（左到右）
  (func eval-args
    [] env => (ok [])
    [a . rest] env =>
      (bind-eval (eval2 a env)
        (lambda (av)
          (bind-eval (eval-args rest env)
            (lambda (rv)
              (ok [av . rv]))))))

  ;; 应用：部分应用 / 过度应用
  (func apply-closure
    params body cenv args =>
      (let pn (list-length params)
        (let an (list-length args)
          (if (= an pn)
              (eval2 body (bind-params params args cenv))
              (if (> pn an)
                  (ok ['closure (drop an params) body (bind-params (take an params) args cenv)])
                  (bind-eval (eval2 body (bind-params params (take pn args) cenv))
                    (lambda (fv)
                      (apply2 fv (drop pn args)))))))))

  ;; 原语（固定参数）
  (func apply-prim
    '+ [a b] => (ok (+ a b))
    '- [a b] => (ok (- a b))
    '* [a b] => (ok (* a b))
    '/ [a b] => (ok (/ a b))
    'cons [a b] => (ok (cons a b))
    'car [p] => (ok (car p))
    'cdr [p] => (ok (cdr p))
    _ _ => (thrown ['bad-prim]))

  (func apply2
    ['closure params body cenv] args => (apply-closure params body cenv args)
    ['prim name] args => (apply-prim name args)
    f _ => (thrown ['not-callable f]))

  ;; 模式匹配：返回 ['ok bindings] 或 ['fail]
  (func match-bind
    ['ok b] k => (k b)
    ['fail] k => ['fail])

  (func pat-match
    '_' _ => ['ok []]
    ['quote sym] v =>
      (if (sym=? v sym) ['ok []] ['fail])
    ['list] v =>
      (if (empty? v) ['ok []] ['fail])
    ['list . ps] v => (pat-match-list ps v)
    ['list-rest p1 prest] v => (pat-match-cons p1 prest v)
    p v =>
      (if (symbol? p)
          ['ok [[p . v]]]
          (if (= p v) ['ok []] ['fail])))

  (func pat-match-list
    [] v => (if (empty? v) ['ok []] ['fail])
    [p . ps] v =>
      (match v
        [x . xs]
          (match-bind (pat-match p x)
            (lambda (b1)
              (match-bind (pat-match-list ps xs)
                (lambda (b2)
                  ['ok (append b1 b2)]))))
        _ ['fail]))

  (func pat-match-cons
    p1 prest v =>
      (match v
        [x . xs]
          (match-bind (pat-match p1 x)
            (lambda (b1)
              (match-bind (pat-match prest xs)
                (lambda (b2)
                  ['ok (append b1 b2)]))))
        _ ['fail]))

  (func eval-match
    v [] env => (thrown ['match-fail v])
    v [pat expr . rest] env =>
      (match (pat-match pat v)
        ['ok binds] (eval2 expr (env-extend-list env binds))
        ['fail] (eval-match v rest env)))

  ;; eval2：严格求值 + try/throw
  (func eval2
    true env => (ok true)
    false env => (ok false)
    ['quote x] env => (ok x)
    ['if t a b] env =>
      (bind-eval (eval2 t env)
        (lambda (tv)
          (if (bool? tv)
              (if tv (eval2 a env) (eval2 b env))
              (thrown ['type-error 'if tv]))))
    ['do a b] env =>
      (bind-eval (eval2 a env)
        (lambda (ignored)
          (eval2 b env)))
    ['lambda params body] env =>
      (ok ['closure params body env])
    ['let x v body] env =>
      (bind-eval (eval2 v env)
        (lambda (vv)
          (eval2 body (env-extend env x vv))))
    ['match v . rules] env =>
      (bind-eval (eval2 v env)
        (lambda (vv)
          (eval-match vv rules env)))
    ['try thunk handler] env =>
      (bind-eval (eval2 thunk env)
        (lambda (tv)
          (match (apply2 tv [])
            ['ok r] ['ok r]
            ['throw err]
              (bind-eval (eval2 handler env)
                (lambda (hv)
                  (apply2 hv [err]))))))
    ['throw v] env =>
      (bind-eval (eval2 v env) thrown)
    [f . args] env =>
      (bind-eval (eval2 f env)
        (lambda (fv)
          (bind-eval (eval-args args env)
            (lambda (av)
              (apply2 fv av)))))
    sym env => (env-lookup env sym) where (symbol? sym)
    atom env => (ok atom))

  (func initial-env
    () =>
      [
        ['+ . ['prim '+]]
        ['- . ['prim '-]]
        ['* . ['prim '*]]
        ['/ . ['prim '/]]
        ['cons . ['prim 'cons]]
        ['car . ['prim 'car]]
        ['cdr . ['prim 'cdr]]
      ])

  (func interpret
    exp => (eval2 exp (initial-env)))
```
