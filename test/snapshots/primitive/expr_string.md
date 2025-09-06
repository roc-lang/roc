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
(block
  (binop_equals
    (lc "name")
    (str_literal_small "luc")
  )
  (binop_equals
    (lc "foo")
    (str_literal_big "hello ${name}")
  )
)
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
; Total type variables: 9
(var #0 _)
(var #1 _)
(var #2 -> #3)
(var #3 Str)
(var #4 _)
(var #5 -> #6)
(var #6 Str)
(var #7 _)
(var #8 _)
~~~
# TYPES
~~~roc
foo : Str
name : Str
~~~
