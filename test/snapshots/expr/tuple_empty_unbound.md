# META
~~~ini
description=Empty tuple literal
type=expr
~~~
# SOURCE
~~~roc
()
~~~
# TOKENS
~~~text
OpenRound CloseRound ~~~
# PARSE
~~~clojure
(tuple_literal)
~~~
# FORMATTED
~~~roc
()
~~~
# EXPECTED
EMPTY TUPLE NOT ALLOWED - tuple_empty_unbound.md:1:1:1:3
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.tuple_literal
)
~~~
# SOLVED
~~~clojure
; Total type variables: 3
(var #0 _)
(var #1 -> #2)
(var #2 tuple)
~~~
# TYPES
~~~roc
~~~
