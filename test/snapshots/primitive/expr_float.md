# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module [foo]
foo = 12.34
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpAssign Float ~~~
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

foo = 12.34
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
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
