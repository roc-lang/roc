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
foo
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.lookup "foo")
~~~
# SOLVED
~~~clojure
(expr :tag lookup :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
