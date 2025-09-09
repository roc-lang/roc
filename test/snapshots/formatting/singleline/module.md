# META
~~~ini
description=Singleline formatting module
type=file
~~~
# SOURCE
~~~roc
module [a, b]

a = 'a'
b = 'a'
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare BlankLine LowerIdent OpAssign SingleQuote LowerIdent OpAssign SingleQuote ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "a")

    (lc "b")
))
(block
  (binop_equals
    (lc "a")
    (str_literal_small "a")
  )
  (binop_equals
    (lc "b")
    (str_literal_small "a")
  )
)
~~~
# FORMATTED
~~~roc
module [a, b]

a = 'a'
b = 'a'
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "a"))
    (Expr.str_literal_small)
  )
  (Stmt.assign
    (pattern (Patt.ident "b"))
    (Expr.str_literal_small)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 10
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 -> #4)
(var #4 Str)
(var #5 _)
(var #6 -> #7)
(var #7 Str)
(var #8 _)
(var #9 _)
~~~
# TYPES
~~~roc
~~~
