# META
~~~ini
description=Simple string literal
type=expr
~~~
# SOURCE
~~~roc
"hello world"
~~~
# TOKENS
~~~text
String ~~~
# PARSE
~~~clojure
(str_literal_big "hello world")
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
(Expr.binop_lte)
~~~
# SOLVED
~~~clojure
(expr :tag binop_lte :type "[True, False]_others")
~~~
# TYPES
~~~roc
[True, False]_others
~~~
