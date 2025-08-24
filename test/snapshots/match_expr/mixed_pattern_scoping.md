# META
~~~ini
description=Match expression with mixed tag and list patterns testing variable scoping
type=expr
~~~
# SOURCE
~~~roc
match data {
    Ok([x, y]) => x + y
    Err(x) => x - 1
    Ok([x]) => x * 2
    Err(y) => y / 2
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpenRound OpenSquare LowerIdent Comma LowerIdent CloseSquare CloseRound OpFatArrow LowerIdent OpPlus LowerIdent UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent OpBinaryMinus Int UpperIdent OpenRound OpenSquare LowerIdent CloseSquare CloseRound OpFatArrow LowerIdent OpStar Int UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent OpSlash Int CloseCurly ~~~
# PARSE
~~~clojure
(match <30 branches>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNDEFINED VARIABLE - mixed_pattern_scoping.md:1:7:1:11
# PROBLEMS
**Parse Error**
at 1:1 to 1:12

**Parse Error**
at 2:16 to 2:16

**Parse Error**
at 3:12 to 3:12

**Parse Error**
at 4:13 to 4:13

**Parse Error**
at 5:12 to 5:12

**Parse Error**
at 1:1 to 6:2

**Parse Error**
at 6:2 to 6:2

**Unsupported Node**
at 1:12 to 6:1

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
