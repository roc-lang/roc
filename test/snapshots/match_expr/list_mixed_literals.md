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
))
~~~
# FORMATTED
~~~roc
match sequence
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:16 to 2:16

**Parse Error**
at 3:15 to 3:15

**Parse Error**
at 4:17 to 4:17

**Parse Error**
at 5:17 to 5:17

**Parse Error**
at 6:8 to 6:8

# CANONICALIZE
~~~clojure
(Expr.dot_num)
~~~
# SOLVED
~~~clojure
(expr :tag dot_num :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
