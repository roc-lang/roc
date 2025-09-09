# META
~~~ini
description=Hexadecimal integer literal
type=expr
~~~
# SOURCE
~~~roc
0xFF
~~~
# TOKENS
~~~text
IntBase ~~~
# PARSE
~~~clojure
(int_literal_big int:<idx:4>)
~~~
# FORMATTED
~~~roc
0xFF
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.int_literal_big)
~~~
# SOLVED
~~~clojure
; Total type variables: 2
(var #0 _)
(var #1 I128)
~~~
# TYPES
~~~roc
~~~
