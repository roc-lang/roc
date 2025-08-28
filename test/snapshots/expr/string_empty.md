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
(Expr.binop_gt)
~~~
# SOLVED
~~~clojure
(expr :tag binop_gt :type "[True, False]_others")
~~~
# TYPES
~~~roc
[True, False]_others
~~~
