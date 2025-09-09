# META
~~~ini
description=Maximum value for i64 (9223372036854775807)
type=expr
~~~
# SOURCE
~~~roc
9223372036854775807
~~~
# TOKENS
~~~text
Int ~~~
# PARSE
~~~clojure
(num_literal_big num:<idx:20>)
~~~
# FORMATTED
~~~roc
9223372036854775807
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
