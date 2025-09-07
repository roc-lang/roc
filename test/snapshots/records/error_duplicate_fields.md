# META
~~~ini
description=Record with duplicate field names (error case)
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", age: 30, name: "Bob", email: "alice@example.com", age: 25 }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int Comma LowerIdent OpColon String Comma LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly ~~~
# PARSE
~~~clojure
(record_literal
  (binop_colon
    (lc "name")
    (str_literal_big "Alice")
  )
  (binop_colon
    (lc "age")
    (num_literal_i32 30)
  )
  (binop_colon
    (lc "name")
    (str_literal_small "Bob")
  )
  (binop_colon
    (lc "email")
    (str_literal_big "alice@example.com")
  )
  (binop_colon
    (lc "age")
    (num_literal_i32 25)
  )
)
~~~
# FORMATTED
~~~roc
{ name: "Alice", age: 30, name: "Bob", email: "alice@example.com", age: 25 }
~~~
# EXPECTED
DUPLICATE RECORD FIELD - error_duplicate_fields.md:1:27:1:31
DUPLICATE RECORD FIELD - error_duplicate_fields.md:1:68:1:71
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.record_field
    (Expr.malformed)
    (Expr.str_literal_big)
  )
  (Expr.record_field
    (Expr.malformed)
    (Expr.num_literal_i32 30)
  )
  (Expr.record_field
    (Expr.malformed)
    (Expr.str_literal_small)
  )
  (Expr.record_field
    (Expr.malformed)
    (Expr.str_literal_big)
  )
  (Expr.record_field
    (Expr.malformed)
    (Expr.num_literal_i32 25)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 19
(var #0 _)
(var #1 _)
(var #2 Str)
(var #3 _)
(var #4 _)
(var #5 Num *)
(var #6 _)
(var #7 _)
(var #8 Str)
(var #9 _)
(var #10 _)
(var #11 Str)
(var #12 _)
(var #13 _)
(var #14 Num *)
(var #15 _)
(var #16 -> #18)
(var #17 {})
(var #18 record)
~~~
# TYPES
~~~roc
~~~
