# META
~~~ini
description=Record with mixed field types
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", age: 30, active: Bool.true, scores: [95, 87, 92], balance: 1250.75 }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int Comma LowerIdent OpColon UpperIdent Dot LowerIdent Comma LowerIdent OpColon OpenSquare Int Comma Int Comma Int CloseSquare Comma LowerIdent OpColon Float CloseCurly ~~~
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
    (lc "active")
    (binop_pipe
      (uc "Bool")
      (dot_lc "true")
    )
  )
  (binop_colon
    (lc "scores")
    (list_literal
      (num_literal_i32 95)
      (num_literal_i32 87)
      (num_literal_i32 92)
    )
  )
  (binop_colon
    (lc "balance")
    (frac_literal_big big:<idx:6>)
  )
)
~~~
# FORMATTED
~~~roc
{
	name: "Alice",
	age: 30,
	active: Bool.true,
	scores: [95, 87, 92],
	balance: 1250.75
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:35 to 1:39

**Unsupported Node**
at 1:54 to 1:66

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
    (Expr.lookup "active")
    (Expr.lambda)
  )
  (Expr.binop_colon
    (Expr.lookup "scores")
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.lookup "balance")
    (Expr.frac_literal_big big:<idx:6>)
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
