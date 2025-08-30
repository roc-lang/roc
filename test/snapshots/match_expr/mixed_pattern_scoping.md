# META
~~~ini
description=Match expression with mixed tag and list patterns testing variable scoping
type=expr
~~~
# SOURCE
~~~roc
match data {
    Ok([x, y]) => x + y
    Err(x) => x - 1
    Ok([x]) => x * 2
    Err(y) => y / 2
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpenRound OpenSquare LowerIdent Comma LowerIdent CloseSquare CloseRound OpFatArrow LowerIdent OpPlus LowerIdent UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent OpBinaryMinus Int UpperIdent OpenRound OpenSquare LowerIdent CloseSquare CloseRound OpFatArrow LowerIdent OpStar Int UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent OpSlash Int CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "data")
)
  (branch1     (binop_thick_arrow
      (apply_uc
        (uc "Ok")
        (list_literal
          (lc "x")
          (lc "y")
        )
      )
      (binop_plus
        (lc "x")
        (lc "y")
      )
    )
)
  (branch2     (binop_thick_arrow
      (apply_uc
        (uc "Err")
        (lc "x")
      )
      (binop_minus
        (lc "x")
        (num_literal_i32 1)
      )
    )
)
  (branch3     (binop_thick_arrow
      (apply_uc
        (uc "Ok")
        (list_literal
          (lc "x")
        )
      )
      (binop_star
        (lc "x")
        (num_literal_i32 2)
      )
    )
)
  (branch4     (binop_thick_arrow
      (apply_uc
        (uc "Err")
        (lc "y")
      )
      (binop_slash
        (lc "y")
        (num_literal_i32 2)
      )
    )
))
~~~
# FORMATTED
~~~roc
match data
	Ok([x, y]) => x + y
	Err(x) => x - 1
	Ok([x]) => x * 2
	Err(y) => y / 2
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:16 to 2:18

**Unsupported Node**
at 3:5 to 3:11

**Unsupported Node**
at 4:13 to 4:15

**Unsupported Node**
at 5:5 to 5:11

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
