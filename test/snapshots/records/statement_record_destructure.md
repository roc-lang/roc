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
      (tuple_literal
        (lc "age")
        (lc "email")
      )
    )
    (lc "person")
  )
)
~~~
# FORMATTED
~~~roc
module []


{ name, (age, email) } = person
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:21

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
