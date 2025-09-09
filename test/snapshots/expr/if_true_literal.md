# META
~~~ini
description=If expression with True boolean literal
type=expr
~~~
# SOURCE
~~~roc
if True 1 else 2
~~~
# TOKENS
~~~text
KwIf UpperIdent Int KwElse Int ~~~
# PARSE
~~~clojure
(if_else
  (condition     (uc "True")
)
  (then     (num_literal_i32 1)
)
  (else     (num_literal_i32 2)
))
~~~
# FORMATTED
~~~roc
if True 1 else 2
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.if_else)
~~~
# SOLVED
~~~clojure
; Total type variables: 6
(var #0 _)
(var #1 -> #5)
(var #2 -> #3)
(var #3 Num *)
(var #4 -> #3)
(var #5 _)
~~~
# TYPES
~~~roc
~~~
