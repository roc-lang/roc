# META
~~~ini
description=Large integer literal
type=expr
~~~
# SOURCE
~~~roc
999999999999999999999999999999
~~~
# TOKENS
~~~text
Int ~~~
# PARSE
~~~clojure
(num_literal_big num:<idx:31>)
~~~
# FORMATTED
~~~roc
999999999999999999999999999999
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.num_literal_big)
~~~
# SOLVED
~~~clojure
; Total type variables: 2
(var #0 _)
(var #1 Num *)
~~~
# TYPES
~~~roc
~~~
