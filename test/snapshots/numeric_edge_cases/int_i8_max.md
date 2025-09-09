# META
~~~ini
description=Maximum value for i8 (127)
type=expr
~~~
# SOURCE
~~~roc
127
~~~
# TOKENS
~~~text
Int ~~~
# PARSE
~~~clojure
(num_literal_i32 127)
~~~
# FORMATTED
~~~roc
127
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.num_literal_i32 127)
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
