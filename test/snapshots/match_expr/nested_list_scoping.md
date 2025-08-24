# META
~~~ini
description=Match expression with nested list patterns testing variable scoping
type=expr
~~~
# SOURCE
~~~roc
match nestedList {
    [[x], [y]] => x + y
    [[x, y]] => x - y  
    [x, [y]] => x * y
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare OpenSquare LowerIdent CloseSquare Comma OpenSquare LowerIdent CloseSquare CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent OpenSquare OpenSquare LowerIdent Comma LowerIdent CloseSquare CloseSquare OpFatArrow LowerIdent OpBinaryMinus LowerIdent OpenSquare LowerIdent Comma OpenSquare LowerIdent CloseSquare CloseSquare OpFatArrow LowerIdent OpStar LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match <21 branches>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNDEFINED VARIABLE - nested_list_scoping.md:1:7:1:17
# PROBLEMS
**Parse Error**
at 1:1 to 1:18

**Parse Error**
at 2:16 to 2:16

**Parse Error**
at 3:14 to 3:14

**Parse Error**
at 4:14 to 4:14

**Parse Error**
at 1:1 to 5:2

**Parse Error**
at 5:2 to 5:2

**Unsupported Node**
at 1:18 to 5:1

**Unsupported Node**
at 5:2 to 5:2

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
