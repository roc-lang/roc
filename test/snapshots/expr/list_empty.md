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
(Expr.binop_or)
~~~
# SOLVED
~~~clojure
(expr :tag binop_or :type "[True, False]_others")
~~~
# TYPES
~~~roc
[True, False]_others
~~~
