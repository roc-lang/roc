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
(Expr.num_literal_i32 0)
~~~
# SOLVED
~~~clojure
(expr :tag num_literal_i32 :type "Num(_size)")
~~~
# TYPES
~~~roc
Num(_size)
~~~
