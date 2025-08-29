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
))
~~~
# FORMATTED
~~~roc
match items
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:21 to 2:21

**Parse Error**
at 3:20 to 3:20

**Parse Error**
at 4:20 to 4:20

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
