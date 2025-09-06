# META
~~~ini
description=Record expression
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", age: 30 }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly ~~~
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
)
~~~
# FORMATTED
~~~roc
{ name: "Alice", age: 30 }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.str_literal_big)
  )
  (Expr.binop_colon
    (Expr.malformed)
    (Expr.num_literal_i32 30)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 9
(var #0 _)
(var #1 _)
(var #2 Str)
(var #3 _)
(var #4 _)
(var #5 Num *)
(var #6 _)
(var #7 -> #8)
(var #8 {})
~~~
# TYPES
~~~roc
~~~
