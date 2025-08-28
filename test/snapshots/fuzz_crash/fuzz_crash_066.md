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

C : [0]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
