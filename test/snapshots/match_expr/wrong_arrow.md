# META
~~~ini
description=Match expression with wrong arrow
type=expr
~~~
# SOURCE
~~~roc
match l {
    [] -> Err(EmptyList)
    [.., e] -> Ok(e)
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare CloseSquare OpArrow UpperIdent OpenRound UpperIdent CloseRound OpenSquare DoubleDot Comma LowerIdent CloseSquare OpArrow UpperIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "l")
))
~~~
# FORMATTED
~~~roc
match l
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:8 to 2:8

**Parse Error**
at 3:8 to 3:8

**Parse Error**
at 3:5 to 3:10

**Parse Error**
at 3:11 to 3:11

**Parse Error**
at 3:13 to 3:13

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
