# META
~~~ini
description=Negative single quote char literal
type=expr
~~~
# SOURCE
~~~roc
-'i'
~~~
# TOKENS
~~~text
OpUnaryMinus SingleQuote ~~~
# PARSE
~~~clojure
(unary_neg <unary_op>)
~~~
# FORMATTED
~~~roc
-'i'
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - negative_single_quote.md:1:1:1:2
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.unary_neg)
~~~
# SOLVED
~~~clojure
; Total type variables: 3
(var #0 _)
(var #1 Str)
(var #2 -> #1)
~~~
# TYPES
~~~roc
~~~
