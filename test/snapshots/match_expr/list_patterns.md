# META
~~~ini
description=Match expression with list patterns including invalid rest pattern
type=expr
~~~
# SOURCE
~~~roc
match numbers {
    [] => acc
    [first, ..rest] => 0 # invalid rest pattern should error
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare CloseSquare OpFatArrow LowerIdent OpenSquare LowerIdent Comma DoubleDot LowerIdent CloseSquare OpFatArrow Int CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "numbers")
)
  (branch1     (binop_thick_arrow
      (list_literal)
      (lc "acc")
    )
)
  (branch2     (binop_thick_arrow
      (list_literal
        (lc "first")
        (unary_double_dot <unary>)
      )
      (num_literal_i32 0)
    )
))
~~~
# FORMATTED
~~~roc
match numbers
	[] => acc
	[first, ..] => 0
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:8 to 2:10

**Unsupported Node**
at 3:5 to 3:20

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
