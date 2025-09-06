# META
~~~ini
description=Maximum value for u8 (255)
type=expr
~~~
# SOURCE
~~~roc
255
~~~
# TOKENS
~~~text
Int ~~~
# PARSE
~~~clojure
(num_literal_i32 255)
~~~
# FORMATTED
~~~roc
255
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.num_literal_i32 255)
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
