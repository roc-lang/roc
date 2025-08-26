# META
~~~ini
description=Record with field update syntax
type=expr
~~~
# SOURCE
~~~roc
{ ..person, age: 31 }
~~~
# TOKENS
~~~text
OpenCurly DoubleDot LowerIdent Comma LowerIdent OpColon Int CloseCurly ~~~
# PARSE
~~~clojure
(record_literal
  (double_dot_lc "person")
)
~~~
# FORMATTED
~~~roc
{ ..person }
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:13

# CANONICALIZE
~~~clojure
(Expr.binop_double_slash)
~~~
# SOLVED
~~~clojure
(expr :tag binop_double_slash :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
