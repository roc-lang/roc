# META
~~~ini
description=If expression with conditional
type=expr
~~~
# SOURCE
~~~roc
if x > 5 "big" else "small"
~~~
# TOKENS
~~~text
KwIf LowerIdent OpGreaterThan Int String KwElse String ~~~
# PARSE
~~~clojure
(if_else <2 branches>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNDEFINED VARIABLE - if_expression.md:1:4:1:5
# PROBLEMS
**Parse Error**
at 1:1 to 1:10

# CANONICALIZE
~~~clojure
(Expr.if_else)
~~~
# SOLVED
~~~clojure
(expr :tag if_else :type "Str")
~~~
# TYPES
~~~roc
Str
~~~
