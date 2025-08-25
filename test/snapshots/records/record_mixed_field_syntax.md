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
  (binop_colon
    (lc "age")
    (num_literal_i32 30)
  )
  (lc "email")
  (binop_colon
    (lc "status")
    (str_literal_big "active")
  )
  (lc "balance")
)
~~~
# FORMATTED
~~~roc
{
	name,
	age: 30,
	email,
	status: "active",
	balance
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.lookup "name")
  (Expr.binop_colon
    (Expr.lookup "age")
    (Expr.num_literal_i32 30)
  )
  (Expr.lookup "email")
  (Expr.binop_colon
    (Expr.lookup "status")
    (Expr.str_literal_big)
  )
  (Expr.lookup "balance")
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
