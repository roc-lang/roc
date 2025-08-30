# META
~~~ini
description=Match expression demonstrating variable shadowing between outer scope and branches
type=expr
~~~
# SOURCE
~~~roc
match (value, other) {
    (Some(x), y) => x + y
    (None, x) => x * 2
}
~~~
# TOKENS
~~~text
KwMatch OpenRound LowerIdent Comma LowerIdent CloseRound OpenCurly OpenRound UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent CloseRound OpFatArrow LowerIdent OpPlus LowerIdent OpenRound UpperIdent Comma LowerIdent CloseRound OpFatArrow LowerIdent OpStar Int CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (tuple_literal
      (lc "value")
      (lc "other")
    )
)
  (branch1     (binop_thick_arrow
      (binop_thick_arrow
        (tuple_literal
          (apply_uc
            (uc "Some")
            (lc "x")
          )
          (lc "y")
        )
        (binop_plus
          (lc "x")
          (apply_lc
            (lc "y")
            (tuple_literal
              (uc "None")
              (lc "x")
            )
          )
        )
      )
      (binop_star
        (lc "x")
        (num_literal_i32 2)
      )
    )
))
~~~
# FORMATTED
~~~roc
match (value, other)
	(Some(x), y) => x + y((None, x)) => x * 2
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:15 to 3:17

# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
(expr :tag match :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
