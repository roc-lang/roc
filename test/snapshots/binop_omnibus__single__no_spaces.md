# META
~~~ini
description=Binop omnibus - singleline - no spaces
type=expr
~~~
# SOURCE
~~~roc
Err(foo)??12>5*5 or 13+2<5 and 10-1>=16 or 12<=3/5
~~~
# TOKENS
~~~text
UpperIdent OpenRound LowerIdent CloseRound OpDoubleQuestion Int OpGreaterThan Int OpStar Int OpOr Int OpPlus Int OpLessThan Int OpAnd Int OpUnaryMinus Int OpGreaterThanOrEq Int OpOr Int OpLessThanOrEq Int OpSlash Int ~~~
# PARSE
~~~clojure
(binop_or
  (binop_or
    (binop_gt
      (binop_double_question
        (apply_uc
          (uc "Err")
          (lc "foo")
        )
        (num_literal_i32 12)
      )
      (binop_star
        (num_literal_i32 5)
        (num_literal_i32 5)
      )
    )
    (binop_and
      (binop_lt
        (binop_plus
          (num_literal_i32 13)
          (num_literal_i32 2)
        )
        (num_literal_i32 5)
      )
      (binop_gte
        (binop_minus
          (num_literal_i32 10)
          (num_literal_i32 1)
        )
        (num_literal_i32 16)
      )
    )
  )
  (binop_lte
    (num_literal_i32 12)
    (binop_slash
      (num_literal_i32 3)
      (num_literal_i32 5)
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNDEFINED VARIABLE - binop_omnibus__single__no_spaces.md:1:5:1:8
INVALID BOOL OPERATION - binop_omnibus__single__no_spaces.md:1:21:1:21
# PROBLEMS
**Unsupported Node**
at 1:1 to 1:13

# CANONICALIZE
~~~clojure
(Expr.binop_or
  (Expr.binop_or
    (Expr.binop_gt
      (Expr.malformed)
      (Expr.binop_star
        (Expr.num_literal_i32 5)
        (Expr.num_literal_i32 5)
      )
    )
    (Expr.binop_and
      (Expr.binop_lt
        (Expr.binop_plus
          (Expr.num_literal_i32 13)
          (Expr.num_literal_i32 2)
        )
        (Expr.num_literal_i32 5)
      )
      (Expr.binop_gte
        (Expr.binop_minus
          (Expr.num_literal_i32 10)
          (Expr.num_literal_i32 1)
        )
        (Expr.num_literal_i32 16)
      )
    )
  )
  (Expr.binop_lte
    (Expr.num_literal_i32 12)
    (Expr.binop_slash
      (Expr.num_literal_i32 3)
      (Expr.num_literal_i32 5)
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag binop_or :type "[True, False]_others")
~~~
# TYPES
~~~roc
[True, False]_others
~~~
