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
(Expr.list_literal)
~~~
# SOLVED
~~~clojure
(expr :tag list_literal :type "List(_a)")
~~~
# TYPES
~~~roc
List(_a)
~~~
