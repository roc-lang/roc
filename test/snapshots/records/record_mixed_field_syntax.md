# META
~~~ini
description=Record construction using mixed shorthand and explicit record fields
type=expr
~~~
# SOURCE
~~~roc
{ name, age: 30, email, status: "active", balance }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent Comma LowerIdent OpColon Int Comma LowerIdent Comma LowerIdent OpColon String Comma LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(record_literal
  (lc "name")
  (tuple_literal
    (binop_colon
      (tuple_literal
        (binop_colon
          (lc "age")
          (num_literal_i32 30)
        )
        (lc "email")
        (lc "status")
      )
      (str_literal_big "active")
    )
    (lc "balance")
  )
)
~~~
# FORMATTED
~~~roc
{ name, ((age: 30, email, status): "active", balance) }
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:51 to 1:51

# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.lookup "name")
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
