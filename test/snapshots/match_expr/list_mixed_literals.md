# META
~~~ini
description=Match expression with mixed literal and variable patterns in lists
type=expr
~~~
# SOURCE
~~~roc
match sequence {
    [0, count] => count
    [1, x, 3] => x
    [42, value] => value
    [first, 99] => first
    [] => 0
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare Int Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare Int Comma LowerIdent Comma Int CloseSquare OpFatArrow LowerIdent OpenSquare Int Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare LowerIdent Comma Int CloseSquare OpFatArrow LowerIdent OpenSquare CloseSquare OpFatArrow Int CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "sequence")
)
  (branch1     (binop_thick_arrow
      (list_literal
        (num_literal_i32 0)
        (lc "count")
      )
      (lc "count")
    )
)
  (branch2     (binop_thick_arrow
      (list_literal
        (num_literal_i32 1)
        (lc "x")
        (num_literal_i32 3)
      )
      (lc "x")
    )
)
  (branch3     (binop_thick_arrow
      (list_literal
        (num_literal_i32 42)
        (lc "value")
      )
      (lc "value")
    )
)
  (branch4     (binop_thick_arrow
      (list_literal
        (lc "first")
        (num_literal_i32 99)
      )
      (lc "first")
    )
)
  (branch5     (binop_thick_arrow
      (list_literal)
      (num_literal_i32 0)
    )
))
~~~
# FORMATTED
~~~roc
match sequence
	[0, count] => count
	[1, x, 3] => x
	[42, value] => value
	[first, 99] => first
	[] => 0
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:16 to 2:18

**Unsupported Node**
at 3:5 to 3:14

**Unsupported Node**
at 4:17 to 4:19

**Unsupported Node**
at 5:5 to 5:16

**Unsupported Node**
at 6:8 to 6:10

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
