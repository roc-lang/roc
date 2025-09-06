# META
~~~ini
description=Record with single field
type=expr
~~~
# SOURCE
~~~roc
{ name: "test" }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "name")
    (str_literal_small "test")
  )
)
~~~
# FORMATTED
~~~roc
name : "test"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 6
(var #0 _)
(var #1 _)
(var #2 Str)
(var #3 _)
(var #4 -> #5)
(var #5 {})
~~~
# TYPES
~~~roc
~~~
