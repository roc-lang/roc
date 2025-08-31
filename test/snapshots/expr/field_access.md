# META
~~~ini
description=Field access expression simple expression
type=expr
~~~
# SOURCE
~~~roc
person.name
~~~
# TOKENS
~~~text
LowerIdent Dot LowerIdent ~~~
# PARSE
~~~clojure
(binop_pipe
  (lc "person")
  (dot_lc "name")
)
~~~
# FORMATTED
~~~roc
person.name
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.lambda (canonicalized))
~~~
# SOLVED
~~~clojure
(expr :tag lambda :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
