# META
~~~ini
description=Simple integer literal canonicalization
type=expr
~~~
# SOURCE
~~~roc
42
~~~
# TOKENS
~~~text
Int ~~~
# PARSE
~~~clojure
(num_literal_i32 42)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_star)
~~~
# SOLVED
~~~clojure
(expr :tag binop_star :type "Num(_a)")
~~~
# TYPES
~~~roc
Num(_a)
~~~
