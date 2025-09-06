# META
~~~ini
description=Maximum value for i16 (32767)
type=expr
~~~
# SOURCE
~~~roc
32767
~~~
# TOKENS
~~~text
Int ~~~
# PARSE
~~~clojure
(num_literal_i32 32767)
~~~
# FORMATTED
~~~roc
32767
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.num_literal_i32 32767)
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
