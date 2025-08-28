# META
~~~ini
description=Simple string interpolation
type=expr
~~~
# SOURCE
~~~roc
"Hello ${name}!"
~~~
# TOKENS
~~~text
String ~~~
# PARSE
~~~clojure
(str_literal_big "Hello ${name}!")
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
(Expr.binop_and)
~~~
# SOLVED
~~~clojure
(expr :tag binop_and :type "[True, False]_others")
~~~
# TYPES
~~~roc
[True, False]_others
~~~
