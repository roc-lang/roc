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
