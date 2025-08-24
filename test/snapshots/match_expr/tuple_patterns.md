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
(match <23 branches>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNDEFINED VARIABLE - tuple_patterns.md:1:7:1:12
UNUSED VARIABLE - tuple_patterns.md:5:9:5:10
# PROBLEMS
**Parse Error**
at 1:1 to 1:13

**Parse Error**
at 2:18 to 2:18

**Parse Error**
at 3:15 to 3:15

**Parse Error**
at 4:15 to 4:15

**Parse Error**
at 5:12 to 5:12

**Parse Error**
at 1:1 to 6:2

**Parse Error**
at 6:2 to 6:2

**Unsupported Node**
at 1:13 to 6:1

**Unsupported Node**
at 6:2 to 6:2

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
