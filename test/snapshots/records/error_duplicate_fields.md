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
{
	name: "Alice",
	age: 30,
	name: "Bob",
	email: "alice@example.com",
	age: 25
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.lookup "name")
    (Expr.str_literal_big)
  )
  (Expr.binop_colon
    (Expr.lookup "age")
    (Expr.num_literal_i32 30)
  )
  (Expr.binop_colon
    (Expr.lookup "name")
    (Expr.str_literal_small)
  )
  (Expr.binop_colon
    (Expr.lookup "email")
    (Expr.str_literal_big)
  )
  (Expr.binop_colon
    (Expr.lookup "age")
    (Expr.num_literal_i32 25)
  )
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
