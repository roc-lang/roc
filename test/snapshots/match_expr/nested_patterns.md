# META
~~~ini
description=Match expression with nested patterns (tags containing records, lists with tags)
type=expr
~~~
# SOURCE
~~~roc
match data {
    Container({ items: [First(x), .. as rest] }) => x + List.len(rest)
    Container({ items: [] }) => 0
    Wrapper([Tag(value), Other(y)]) => value + y
    Simple(x) => x
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpenRound OpenCurly LowerIdent OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma DoubleDot KwAs LowerIdent CloseSquare CloseCurly CloseRound OpFatArrow LowerIdent OpPlus UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound UpperIdent OpenRound OpenCurly LowerIdent OpColon OpenSquare CloseSquare CloseCurly CloseRound OpFatArrow Int UpperIdent OpenRound OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare CloseRound OpFatArrow LowerIdent OpPlus LowerIdent UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "data")
))
~~~
# FORMATTED
~~~roc
match data
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:38 to 2:38

**Parse Error**
at 2:24 to 2:41

**Parse Error**
at 2:45 to 2:45

**Parse Error**
at 2:50 to 2:50

**Parse Error**
at 3:30 to 3:30

**Parse Error**
at 4:37 to 4:37

**Parse Error**
at 5:15 to 5:15

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
