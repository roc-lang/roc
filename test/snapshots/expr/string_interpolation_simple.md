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
"Hello ${name}!"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.str_literal_big)
~~~
# SOLVED
~~~clojure
(expr :tag str_literal_big :type "Str")
~~~
# TYPES
~~~roc
Str
~~~
