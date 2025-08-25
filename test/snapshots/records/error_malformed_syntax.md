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
    (tuple_literal
      (str_literal_big "Alice")
      (malformed malformed:expr_unexpected_token)
    )
  )
  (num_literal_i32 30)
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc
{ name: ("Alice", <malformed>), 30, <malformed> }
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:18 to 1:18

**Parse Error**
at 1:24 to 1:24

**Parse Error**
at 1:1 to 1:26

**Unsupported Node**
at 1:1 to 1:1

**Unsupported Node**
at 1:24 to 1:24

# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.lookup "name")
    (Expr.malformed)
  )
  (Expr.num_literal_i32 30)
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
