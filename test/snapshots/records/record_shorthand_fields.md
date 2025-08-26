# META
~~~ini
description=Record construction using shorthand field syntax
type=expr
~~~
# SOURCE
~~~roc
{ name, age, email, active }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent Comma LowerIdent Comma LowerIdent Comma LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(record_literal
  (lc "name")
  (lc "age")
  (lc "email")
  (lc "active")
)
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
