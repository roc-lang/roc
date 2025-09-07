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
[]
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
; Total type variables: 4
(var #0 _)
(var #1 -> #3)
(var #2 _)
(var #3 List #2)
~~~
# TYPES
~~~roc
~~~
