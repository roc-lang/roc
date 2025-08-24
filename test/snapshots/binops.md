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
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 16:1 to 16:1

**Parse Error**
at 16:2 to 16:2

**Unsupported Node**
at 1:1 to 1:1

# CANONICALIZE
~~~clojure
(Stmt.malformed)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# No expression found
~~~
