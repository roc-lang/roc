# META
~~~ini
description=Integer literal with underscores
type=expr
~~~
# SOURCE
~~~roc
1_000_000
~~~
# TOKENS
~~~text
Int Underscore Int Underscore Int ~~~
# PARSE
~~~clojure
(num_literal_i32 1)
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
(Expr.binop_slash)
~~~
# SOLVED
~~~clojure
(expr :tag binop_slash :type "Num(_a)")
~~~
# TYPES
~~~roc
Num(_a)
~~~
