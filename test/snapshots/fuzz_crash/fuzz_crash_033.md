# META
~~~ini
description=minimal reproduction of record parsing index out of bounds crash
type=expr
~~~
# SOURCE
~~~roc
{ i, Complete]
~~~
# TOKENS
~~~text
OpenCurly LowerIdent Comma UpperIdent CloseSquare ~~~
# PARSE
~~~clojure
(record_literal
  (lc "i")
  (uc "Complete")
)
~~~
# FORMATTED
~~~roc
{ i, Complete }
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:14

# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.lookup "i")
  (Expr.apply_tag)
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
