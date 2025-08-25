# META
~~~ini
description=unary_negation_access
type=expr
~~~
# SOURCE
~~~roc
-rec1.field
~~~
# TOKENS
~~~text
OpUnaryMinus LowerIdent Dot LowerIdent ~~~
# PARSE
~~~clojure
(unary_neg <unary>)
~~~
# FORMATTED
~~~roc
-rec1 | .field
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
(expr :tag unary_neg :type "_arg -> _ret")
~~~
# TYPES
~~~roc
_arg -> _ret
~~~
