# META
~~~ini
description=Binop omnibus - singleline
type=expr
~~~
# SOURCE
~~~roc
Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
~~~
# TOKENS
~~~text
UpperIdent OpenRound LowerIdent CloseRound OpDoubleQuestion Int OpGreaterThan Int OpStar Int OpOr Int OpPlus Int OpLessThan Int OpAnd Int OpBinaryMinus Int OpGreaterThanOrEq Int OpOr Int OpLessThanOrEq Int OpSlash Int ~~~
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
(Err(foo) ?? 12 > 5 * 5 || 13 + 2 < 5 && 10 - 1 >= 16) || 12 <= 3 / 5
~~~
# EXPECTED
UNDEFINED VARIABLE - binop_omnibus__singleline.md:1:5:1:8
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **foo** in this scope.
Is there an **import** or **exposing** missing up-top?

**binop_omnibus__singleline.md:1:5:1:8:**
```roc
Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
```
    ^^^


# CANONICALIZE
~~~clojure
(Expr.binop_or
  (Expr.binop_or
    (Expr.binop_gt
      (Expr.binop_double_question
        (Expr.tag_applied)
        (Expr.num_literal_i32 12)
      )
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
; Total type variables: 28
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 Num *)
(var #5 -> #8)
(var #6 -> #7)
(var #7 -> #8)
(var #8 -> #9)
(var #9 -> #20)
(var #10 -> #11)
(var #11 -> #12)
(var #12 -> #13)
(var #13 -> #14)
(var #14 -> #19)
(var #15 -> #16)
(var #16 -> #17)
(var #17 -> #18)
(var #18 -> #19)
(var #19 -> #20)
(var #20 -> #21)
(var #21 -> #26)
(var #22 -> #25)
(var #23 -> #24)
(var #24 -> #25)
(var #25 -> #26)
(var #26 -> #27)
(var #27 Num *)
~~~
# TYPES
~~~roc
~~~
