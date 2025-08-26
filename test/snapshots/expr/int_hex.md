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
Int LowerIdent ~~~
# PARSE
~~~clojure
(num_literal_i32 0)
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
(expr :tag binop_star :type "Num(_size)")
~~~
# TYPES
~~~roc
Num(_size)
~~~
