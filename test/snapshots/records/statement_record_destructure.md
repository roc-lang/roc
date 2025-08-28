# META
~~~ini
description=Record destructuring in assignment statement
type=file
~~~
# SOURCE
~~~roc
module []

{ name, age, email } = person
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare OpenCurly LowerIdent Comma LowerIdent Comma LowerIdent CloseCurly OpAssign LowerIdent ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (record_literal
      (lc "name")
      (lc "age")
      (lc "email")
    )
    (lc "person")
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.record_literal
      (Expr.lookup "name")
      (Expr.lookup "age")
      (Expr.lookup "email")
    )
    (Expr.lookup "person")
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
