# META
~~~ini
description=Simple definition with type annotation
type=file
~~~
# SOURCE
~~~roc
module [foo]

foo : Str
foo = "one"
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign String ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "foo")
))
~~~
# FORMATTED
~~~roc
module [foo]


foo : Str
foo = "one"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
