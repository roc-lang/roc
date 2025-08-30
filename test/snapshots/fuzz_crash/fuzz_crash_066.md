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
(module-header)
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
(Expr.record_literal
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.list_literal)
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
