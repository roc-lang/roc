# META
~~~ini
description=num_bang_amp_z_dot_t
type=expr
~~~
# SOURCE
~~~roc
4
!
&z.t
~~~
# TOKENS
~~~text
Int OpBang OpAmpersand LowerIdent Dot LowerIdent ~~~
# PARSE
~~~clojure
(num_literal_i32 4)
~~~
# FORMATTED
~~~roc
4
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.num_literal_i32 4)
~~~
# SOLVED
~~~clojure
(expr :tag num_literal_i32 :type "Num(_size)")
~~~
# TYPES
~~~roc
Num(_size)
~~~
