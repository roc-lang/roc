# META
~~~ini
description=Match expression demonstrating variable shadowing between outer scope and branches
type=expr
~~~
# SOURCE
~~~roc
match (value, other) {
    (Some(x), y) => x + y
    (None, x) => x * 2
}
~~~
# TOKENS
~~~text
KwMatch OpenRound LowerIdent Comma LowerIdent CloseRound OpenCurly OpenRound UpperIdent OpenRound LowerIdent CloseRound Comma LowerIdent CloseRound OpFatArrow LowerIdent OpPlus LowerIdent OpenRound UpperIdent Comma LowerIdent CloseRound OpFatArrow LowerIdent OpStar Int CloseCurly ~~~
# PARSE
~~~clojure
(match <0 branches>)
~~~
# FORMATTED
~~~roc
match (value, other)
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:18 to 2:21

**Parse Error**
at 3:15 to 3:18

**Parse Error**
at 1:22 to 4:2

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
