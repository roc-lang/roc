# META
~~~ini
description=Match expression with tuple destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match coord {
    (Zero, Zero) => "origin"
    (x, Zero) => x
    (Zero, y) => y
    (x, y) => x
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenRound UpperIdent Comma UpperIdent CloseRound OpFatArrow String OpenRound LowerIdent Comma UpperIdent CloseRound OpFatArrow LowerIdent OpenRound UpperIdent Comma LowerIdent CloseRound OpFatArrow LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpFatArrow LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "coord")
))
~~~
# FORMATTED
~~~roc
match coord
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:18 to 2:21

**Parse Error**
at 3:15 to 3:18

**Parse Error**
at 4:15 to 4:18

**Parse Error**
at 5:12 to 5:15

**Parse Error**
at 1:13 to 6:2

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
