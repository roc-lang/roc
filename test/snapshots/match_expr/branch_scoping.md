# META
~~~ini
description=Comprehensive test for match branch scoping with variable isolation
type=expr
~~~
# SOURCE
~~~roc
match result {
    Ok(value) => value + 1
    Err(value) => value - 1
    Ok(different) => different * 2
    Err(different) => different / 2
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent OpPlus Int UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent OpBinaryMinus Int UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent OpStar Int UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent OpSlash Int CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "result")
)
  (branch1     (binop_thick_arrow
      (apply_uc
        (uc "Ok")
        (lc "value")
      )
      (binop_plus
        (lc "value")
        (num_literal_i32 1)
      )
    )
)
  (branch2     (binop_thick_arrow
      (apply_uc
        (uc "Err")
        (lc "value")
      )
      (binop_minus
        (lc "value")
        (num_literal_i32 1)
      )
    )
)
  (branch3     (binop_thick_arrow
      (apply_uc
        (uc "Ok")
        (lc "different")
      )
      (binop_star
        (lc "different")
        (num_literal_i32 2)
      )
    )
)
  (branch4     (binop_thick_arrow
      (apply_uc
        (uc "Err")
        (lc "different")
      )
      (binop_slash
        (lc "different")
        (num_literal_i32 2)
      )
    )
))
~~~
# FORMATTED
~~~roc
match result
	Ok(value) => value + 1
	Err(value) => value - 1
	Ok(different) => different * 2
	Err(different) => different / 2
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:15 to 2:17

**Unsupported Node**
at 3:5 to 3:15

**Unsupported Node**
at 4:19 to 4:21

**Unsupported Node**
at 5:5 to 5:19

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
