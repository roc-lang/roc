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
(block
  (binop_equals
    (lc "foo")
    (frac_literal_small 12.34)
  )
)
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
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.frac_literal_small 12.34)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 6
(var #0 _)
(var #1 _)
(var #2 -> #3)
(var #3 F64)
(var #4 _)
(var #5 _)
~~~
# TYPES
~~~roc
foo : F64
~~~
