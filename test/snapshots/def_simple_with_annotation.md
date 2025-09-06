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
(block
  (binop_colon
    (lc "foo")
    (uc "Str")
  )
  (binop_equals
    (lc "foo")
    (str_literal_small "one")
  )
)
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
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "foo"))
    (type type_3)
  )
  (Stmt.assign
    (pattern (Patt.ident "foo"))
    (Expr.str_literal_small)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 9
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 -> #6)
(var #6 Str)
(var #7 _)
(var #8 _)
~~~
# TYPES
~~~roc
foo : Str
~~~
