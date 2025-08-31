# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module [foo]
foo = 42
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "foo")
))
~~~
# FORMATTED
~~~roc
module [foo]foo = 42
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.num_literal_i32 42)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
