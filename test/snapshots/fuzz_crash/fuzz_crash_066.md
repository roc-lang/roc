# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module []

C:[0]
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare UpperIdent OpColon OpenSquare Int CloseSquare ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (uc "C")
    (list_literal
      (num_literal_i32 0)
    )
  )
)
~~~
# FORMATTED
~~~roc
module []


C: [0]
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:3 to 3:5

# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag record_literal :type "{}")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
