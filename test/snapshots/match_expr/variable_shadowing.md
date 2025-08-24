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
(match <19 branches>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNDEFINED VARIABLE - variable_shadowing.md:1:8:1:13
UNDEFINED VARIABLE - variable_shadowing.md:1:15:1:20
# PROBLEMS
**Parse Error**
at 1:1 to 1:22

**Parse Error**
at 2:18 to 2:18

**Parse Error**
at 3:15 to 3:15

**Parse Error**
at 1:1 to 4:2

**Parse Error**
at 4:2 to 4:2

**Unsupported Node**
at 1:20 to 1:21

**Unsupported Node**
at 1:22 to 4:1

**Unsupported Node**
at 4:2 to 4:2

# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
(expr :tag match :type "Error")
~~~
# TYPES
~~~roc
Error
~~~
