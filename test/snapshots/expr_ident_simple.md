# META
~~~ini
description=Simple identifier lookup canonicalization
type=expr
~~~
# SOURCE
~~~roc
foo
~~~
# TOKENS
~~~text
LowerIdent ~~~
# PARSE
~~~clojure
(lc "foo")
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
