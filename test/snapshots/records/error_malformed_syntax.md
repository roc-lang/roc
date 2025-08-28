# META
~~~ini
description=Malformed record syntax (error case)
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String Comma OpColon Int Comma Comma LowerIdent OpColon Comma LowerIdent UpperIdent Dot LowerIdent Comma String OpColon LowerIdent Comma Int OpColon String Comma OpColon CloseCurly ~~~
# PARSE
~~~clojure
(record_literal
  (binop_colon
    (lc "name")
    (str_literal_big "Alice")
  )
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc
{ name : "Alice" }
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:18 to 1:18

**Parse Error**
at 1:1 to 1:20

# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.lookup "name")
    (Expr.str_literal_big)
  )
  (Expr.malformed)
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
