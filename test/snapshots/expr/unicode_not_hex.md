# META
~~~ini
description=Unicode not hex (should error))
type=expr
~~~
# SOURCE
~~~roc
"abc\u(zzzz)def"
~~~
# TOKENS
~~~text
String ~~~
# PARSE
~~~clojure
(str_literal_big "abcu(zzzz)def")
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
