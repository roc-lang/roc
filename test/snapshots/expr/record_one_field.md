# META
~~~ini
description=Single field record expression
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice" }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "name")
    (str_literal_big "Alice")
  )
)
~~~
# FORMATTED
~~~roc
name : "Alice"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
