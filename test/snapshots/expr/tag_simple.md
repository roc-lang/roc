# META
~~~ini
description=Simple tag literal
type=expr
~~~
# SOURCE
~~~roc
MyTag
~~~
# TOKENS
~~~text
UpperIdent ~~~
# PARSE
~~~clojure
(uc "MyTag")
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
(Expr.apply_tag)
~~~
# SOLVED
~~~clojure
(expr :tag apply_tag :type "[]_others")
~~~
# TYPES
~~~roc
[]_others
~~~
