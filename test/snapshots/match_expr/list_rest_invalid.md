# META
~~~ini
description=Match expression with invalid (old style) list rest patterns should error
type=expr
~~~
# SOURCE
~~~roc
match items {
    [first, ..rest] => 0 # invalid rest pattern should error
    [..rest, last] => 1 # invalid rest pattern should error
    [x, ..rest, y] => 2 # invalid rest pattern should error
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare LowerIdent Comma DoubleDot LowerIdent CloseSquare OpFatArrow Int OpenSquare DoubleDot LowerIdent Comma LowerIdent CloseSquare OpFatArrow Int OpenSquare LowerIdent Comma DoubleDot LowerIdent Comma LowerIdent CloseSquare OpFatArrow Int CloseCurly ~~~
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
      (num_literal_i32 0)
    )
)
  (branch2     (binop_thick_arrow
      (list_literal
        (unary_double_dot <unary>)
        (lc "last")
      )
      (num_literal_i32 1)
    )
)
  (branch3     (binop_thick_arrow
      (list_literal
        (lc "x")
        (unary_double_dot <unary>)
        (lc "y")
      )
      (num_literal_i32 2)
    )
))
~~~
# FORMATTED
~~~roc
match items
	[first, ..] => 0
	[.., last] => 1
	[x, .., y] => 2
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
