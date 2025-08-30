# META
~~~ini
description=Match expression with nested list patterns testing variable scoping
type=expr
~~~
# SOURCE
~~~roc
match nestedList {
    [[x], [y]] => x + y
    [[x, y]] => x - y  
    [x, [y]] => x * y
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare OpenSquare LowerIdent CloseSquare Comma OpenSquare LowerIdent CloseSquare CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent OpenSquare OpenSquare LowerIdent Comma LowerIdent CloseSquare CloseSquare OpFatArrow LowerIdent OpBinaryMinus LowerIdent OpenSquare LowerIdent Comma OpenSquare LowerIdent CloseSquare CloseSquare OpFatArrow LowerIdent OpStar LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "nestedList")
)
  (branch1     (binop_thick_arrow
      (list_literal
        (list_literal
          (lc "x")
        )
        (list_literal
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
      (list_literal
        (list_literal
          (lc "x")
          (lc "y")
        )
      )
      (binop_minus
        (lc "x")
        (lc "y")
      )
    )
)
  (branch3     (binop_thick_arrow
      (list_literal
        (lc "x")
        (list_literal
          (lc "y")
        )
      )
      (binop_star
        (lc "x")
        (lc "y")
      )
    )
))
~~~
# FORMATTED
~~~roc
match nestedList
	[[x], [y]] => x + y
	[[x, y]] => x - y
	[x, [y]] => x * y
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:16 to 2:18

**Unsupported Node**
at 3:5 to 3:13

**Unsupported Node**
at 4:14 to 4:16

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
