# META
~~~ini
description=Record update syntax
type=expr
~~~
# SOURCE
~~~roc
{ ..person, age: 31, active: True }
~~~
# TOKENS
~~~text
OpenCurly DoubleDot LowerIdent Comma LowerIdent OpColon Int Comma LowerIdent OpColon UpperIdent CloseCurly ~~~
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
(Expr.record_literal
  (Expr.unary_double_dot)
)
~~~
# SOLVED
~~~clojure
(expr :tag record_literal :type "{}")
~~~
# TYPES
~~~roc
{}
~~~
