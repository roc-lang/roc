# META
~~~ini
description=Empty list literal
type=expr
~~~
# SOURCE
~~~roc
[]
~~~
# TOKENS
~~~text
OpenSquare CloseSquare ~~~
# PARSE
~~~clojure
(list_literal)
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
(Expr.binop_double_question)
~~~
# SOLVED
~~~clojure
(expr :tag binop_double_question :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
