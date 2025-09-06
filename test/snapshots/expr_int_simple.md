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
42
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.num_literal_i32 42)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~
