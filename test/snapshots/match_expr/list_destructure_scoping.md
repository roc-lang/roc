# META
~~~ini
description=Match expression with various list destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match list {
    [first] => first
    [first, second] => first + second
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "list")
)
  (branch1     (binop_thick_arrow
      (list_literal
        (lc "first")
      )
      (lc "first")
    )
)
  (branch2     (binop_thick_arrow
      (list_literal
        (lc "first")
        (lc "second")
      )
      (binop_plus
        (lc "first")
        (lc "second")
      )
    )
))
~~~
# FORMATTED
~~~roc
match list
	[first] => first
	[first, second] => first + second
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:13 to 2:15

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
