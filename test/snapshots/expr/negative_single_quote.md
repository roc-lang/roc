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
(unary_neg <unary>)
~~~
# FORMATTED
~~~roc
NO CHANGE
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
(expr :tag unary_neg :type "Str")
~~~
# TYPES
~~~roc
Str
~~~
