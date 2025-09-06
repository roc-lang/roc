# META
~~~ini
description=Simple qualified tag test
type=file
~~~
# SOURCE
~~~roc
module [Color]

Color := [Red, Blue]

test = Color.Red
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent CloseSquare BlankLine UpperIdent OpColonEqual OpenSquare UpperIdent Comma UpperIdent CloseSquare BlankLine LowerIdent OpAssign UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "Color")
))
(block
  (binop_colon_equals
    (uc "Color")
    (list_literal
      (uc "Red")
      (uc "Blue")
    )
  )
  (binop_equals
    (lc "test")
    (binop_dot
      (uc "Color")
      (uc "Red")
    )
  )
)
~~~
# FORMATTED
~~~roc
module [Color]

Color := [Red, Blue]
test = (Color.Red)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.opaque_type)
  (Stmt.assign
    (pattern (Patt.ident "test"))
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 13
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 -> #10)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
~~~
# TYPES
~~~roc
test : _a
~~~
