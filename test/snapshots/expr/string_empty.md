# META
~~~ini
description=Empty string literal
type=expr
~~~
# SOURCE
~~~roc
""
~~~
# TOKENS
~~~text
String ~~~
# PARSE
~~~clojure
(str_literal_small "")
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
(Expr.str_literal_small)
~~~
# SOLVED
~~~clojure
(expr :tag str_literal_small :type "Str")
~~~
# TYPES
~~~roc
Str
~~~
