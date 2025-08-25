# META
~~~ini
description=unary_negation
type=expr
~~~
# SOURCE
~~~roc
-foo
~~~
# TOKENS
~~~text
OpUnaryMinus LowerIdent ~~~
# PARSE
~~~clojure
(unary_neg <unary>)
~~~
# FORMATTED
~~~roc
-foo
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.unary_neg)
~~~
# SOLVED
~~~clojure
(expr :tag unary_neg :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
