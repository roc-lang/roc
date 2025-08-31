# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module [foo]
name = "luc"
foo = "hello ${name}"
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpAssign String LowerIdent OpAssign String ~~~
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

name = "luc"
foo = "hello ${name}"
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
