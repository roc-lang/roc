# META
~~~ini
description=Match expression with list rest patterns testing variable scoping
type=expr
~~~
# SOURCE
~~~roc
match items {
    [first, ..rest] => first + 1
    [..rest, last] => last + 2
    [x, ..rest, y] => x + y
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare LowerIdent Comma DoubleDot LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus Int OpenSquare DoubleDot LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus Int OpenSquare LowerIdent Comma DoubleDot LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "items")
)
  (branch1     (binop_thick_arrow
      (list_literal
        (lc "first")
        (unary_double_dot <unary>)
      )
      (binop_plus
        (lc "first")
        (num_literal_i32 1)
      )
    )
)
  (branch2     (binop_thick_arrow
      (list_literal
        (unary_double_dot <unary>)
        (lc "last")
      )
      (binop_plus
        (lc "last")
        (num_literal_i32 2)
      )
    )
)
  (branch3     (binop_thick_arrow
      (list_literal
        (lc "x")
        (unary_double_dot <unary>)
        (lc "y")
      )
      (binop_plus
        (lc "x")
        (lc "y")
      )
    )
))
~~~
# FORMATTED
~~~roc
match items
	[first, ..] => first + 1
	[.., last] => last + 2
	[x, .., y] => x + y
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:21 to 2:23

**Unsupported Node**
at 3:5 to 3:19

**Unsupported Node**
at 4:20 to 4:22

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
