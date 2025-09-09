# META
~~~ini
description=Integer zero
type=expr
~~~
# SOURCE
~~~roc
0
~~~
# TOKENS
~~~text
Int ~~~
# PARSE
~~~clojure
(num_literal_i32 0)
~~~
# FORMATTED
~~~roc
0
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.num_literal_i32 0)
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
