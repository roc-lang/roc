# META
~~~ini
description=List rest patterns with proper variable scoping across branches
type=expr
~~~
# SOURCE
~~~roc
match data {
    [..items] => 1
    [first, ..items] => first
    [..items, last] => last
    [first, ..items, last] => first + last
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare DoubleDot LowerIdent CloseSquare OpFatArrow Int OpenSquare LowerIdent Comma DoubleDot LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare DoubleDot LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare LowerIdent Comma DoubleDot LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "data")
)
  (branch1     (binop_thick_arrow
      (list_literal
        (unary_double_dot <unary>)
      )
      (num_literal_i32 1)
    )
)
  (branch2     (binop_thick_arrow
      (list_literal
        (lc "first")
        (unary_double_dot <unary>)
      )
      (lc "first")
    )
)
  (branch3     (binop_thick_arrow
      (list_literal
        (unary_double_dot <unary>)
        (lc "last")
      )
      (lc "last")
    )
)
  (branch4     (binop_thick_arrow
      (list_literal
        (lc "first")
        (unary_double_dot <unary>)
        (lc "last")
      )
      (binop_plus
        (lc "first")
        (lc "last")
      )
    )
))
~~~
# FORMATTED
~~~roc
match data
	[..] => 1
	[first, ..] => first
	[.., last] => last
	[first, .., last] => first + last
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:15 to 2:17

**Unsupported Node**
at 3:5 to 3:21

**Unsupported Node**
at 4:21 to 4:23

**Unsupported Node**
at 5:5 to 5:27

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
