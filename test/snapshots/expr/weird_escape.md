# META
~~~ini
description=Weird escape (should error)
type=expr
~~~
# SOURCE
~~~roc
"abc\qdef"
~~~
# TOKENS
~~~text
String ~~~
# PARSE
~~~clojure
(str_literal_big "abcqdef")
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
