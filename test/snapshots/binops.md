# META
~~~ini
description=Binops collection
type=expr
~~~
# SOURCE
~~~roc
(
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
    Bool.True and Bool.False,
    Bool.False or Bool.True,
    None ?? 0,
)
~~~
# TOKENS
~~~text
OpenRound Int OpPlus Int Comma Int OpBinaryMinus Int Comma Int OpStar Int Comma Int OpSlash Int Comma Int OpLessThan Int Comma Int OpGreaterThan Int Comma Int OpLessThanOrEq Int Comma Int OpGreaterThanOrEq Int Comma Int OpEquals Int Comma Int OpNotEquals Int Comma Int OpDoubleSlash Int Comma UpperIdent Dot UpperIdent OpAnd UpperIdent Dot UpperIdent Comma UpperIdent Dot UpperIdent OpOr UpperIdent Dot UpperIdent Comma UpperIdent OpDoubleQuestion Int Comma CloseRound ~~~
# PARSE
~~~clojure
(tuple_literal
  (binop_plus
    (num_literal_i32 4)
    (num_literal_i32 2)
  )
  (binop_minus
    (num_literal_i32 4)
    (num_literal_i32 2)
  )
  (binop_star
    (num_literal_i32 4)
    (num_literal_i32 2)
  )
  (binop_slash
    (num_literal_i32 4)
    (num_literal_i32 2)
  )
  (binop_lt
    (num_literal_i32 4)
    (num_literal_i32 2)
  )
  (binop_gt
    (num_literal_i32 4)
    (num_literal_i32 2)
  )
  (binop_lte
    (num_literal_i32 4)
    (num_literal_i32 2)
  )
  (binop_gte
    (num_literal_i32 4)
    (num_literal_i32 2)
  )
  (binop_double_equals
    (num_literal_i32 4)
    (num_literal_i32 2)
  )
  (binop_not_equals
    (num_literal_i32 4)
    (num_literal_i32 2)
  )
  (binop_double_slash
    (num_literal_i32 4)
    (num_literal_i32 2)
  )
  (binop_and
    (binop_pipe
      (uc "Bool")
      (uc "True")
    )
    (binop_pipe
      (uc "Bool")
      (uc "False")
    )
  )
  (binop_or
    (binop_pipe
      (uc "Bool")
      (uc "False")
    )
    (binop_pipe
      (uc "Bool")
      (uc "True")
    )
  )
  (binop_double_question
    (uc "None")
    (num_literal_i32 0)
  )
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc
(
	4 + 2,
	4 - 2,
	4 * 2,
	4 / 2,
	4 < 2,
	4 > 2,
	4 <= 2,
	4 >= 2,
	4 == 2,
	4 != 2,
	4 // 2,
	Bool.True && Bool.False,
	Bool.False || Bool.True,
	None ?? 0,
)
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 16:1 to 16:2

**Parse Error**
at 16:2 to 16:2

# CANONICALIZE
~~~clojure
(Expr.tuple_literal
  (Expr.binop_plus
    (Expr.num_literal_i32 4)
    (Expr.num_literal_i32 2)
  )
  (Expr.binop_minus
    (Expr.num_literal_i32 4)
    (Expr.num_literal_i32 2)
  )
  (Expr.binop_star
    (Expr.num_literal_i32 4)
    (Expr.num_literal_i32 2)
  )
  (Expr.binop_slash
    (Expr.num_literal_i32 4)
    (Expr.num_literal_i32 2)
  )
  (Expr.binop_lt
    (Expr.num_literal_i32 4)
    (Expr.num_literal_i32 2)
  )
  (Expr.binop_gt
    (Expr.num_literal_i32 4)
    (Expr.num_literal_i32 2)
  )
  (Expr.binop_lte
    (Expr.num_literal_i32 4)
    (Expr.num_literal_i32 2)
  )
  (Expr.binop_gte
    (Expr.num_literal_i32 4)
    (Expr.num_literal_i32 2)
  )
  (Expr.binop_double_equals
    (Expr.num_literal_i32 4)
    (Expr.num_literal_i32 2)
  )
  (Expr.binop_not_equals
    (Expr.num_literal_i32 4)
    (Expr.num_literal_i32 2)
  )
  (Expr.binop_double_slash
    (Expr.num_literal_i32 4)
    (Expr.num_literal_i32 2)
  )
  (Expr.binop_and
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
  )
  (Expr.binop_or
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
    (Expr.module_access
      (Expr.malformed)
      (Expr.malformed)
    )
  )
  (Expr.binop_double_question
    (Expr.apply_tag)
    (Expr.num_literal_i32 0)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag tuple_literal :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
