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
module [foo]name = "luc"
foo = "hello ${name}"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "name"))
    (Expr.str_literal_small)
  )
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.str_literal_big)
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
