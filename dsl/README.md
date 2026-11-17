# go.blog template DSL

A tiny SXML-shaped template language compiled by TA (`dsl/reader.ta`,
`dsl/expand.ta`, `dsl/main.ta`). It builds composable `html` Node trees;
serialization happens once in the TA core via `html.render_list`.

## Build

```sh
tools/build-dsl.sh [entry.ta] [out.kl]
```

The script builds the compiler, compiles `dsl/templates.dsl` to
`build/templates.kl` plus the external stubs in `src-ta/lib/template.ta`,
emits KL for the TA entry, and splices them into one runnable KL image.

## Syntax

```lisp
(def site-url "https://tiancaiamao.github.io")

(template page (title body)
  (html (@ lang "zh_CN")
    (head (title ,title))
    (body ,body)))
```

- `def <name> <expr>` defines a zero-arg value; bare references compile to
  KL calls.
- `(template <name> (<params>) <node>...)` defines a template. Its KL name is
  `template.<name>`; the generated TA stub declares it as `external fn`.
- Nodes: strings/ints become Text, symbols plus children become Elem.
  Attributes use the flat form `(@ class "x" id ,title)`; the nested SXML
  form `(@ (class "x") (id ,title))` is also accepted (annotations ignored).
- Expression sugar inside unquotes and attribute values: `cat`, `list`,
  `let`, `if`, `for`, and `lambda`.
- Reader sugar: `` `x `` groups nodes, `,x` inserts one value, `,@xs`
  splices a Node list. `;` and `//` start line comments.
- Extra node forms: `raw`, `comment`, `cdata`, `*PI*`, `*ENTITY*`, `*TOP*`.

A template's body is a sequence of nodes, so its value is always a
`List(Node)` (use `html.render_list` in TA, or splice it with `,@` in another
template).

`dsl/features.dsl` plus `src-ta/test/dsl_test.sh` is a minimal regression
fixture for backquote, splicing and `def` references.