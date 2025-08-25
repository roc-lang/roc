# META
~~~ini
description=Match expression with rest patterns in middle position
type=expr
~~~
# SOURCE
~~~roc
match items {
    [first, .., last] => first + last
    [a, b, .. as middle, x, y] => a + b + x + y  
    [single] => single
    [] => 0
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare LowerIdent Comma DoubleDot Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent OpenSquare LowerIdent Comma LowerIdent Comma DoubleDot KwAs LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent OpPlus LowerIdent OpPlus LowerIdent OpenSquare LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare CloseSquare OpFatArrow Int CloseCurly ~~~
# PARSE
~~~clojure
(match <25 branches>)
~~~
# FORMATTED
~~~roc
when items is { [(first, <unary_double_dot>)], last, <malformed>, <malformed>, first + last, [(a, b, <unary_double_dot>)], middle, (x, y) } -> <malformed> => ((a + b) + x) + y
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:13

**Parse Error**
at 2:15 to 2:15

**Parse Error**
at 2:5 to 2:17

**Parse Error**
at 2:21 to 2:21

**Parse Error**
at 2:23 to 2:23

**Parse Error**
at 3:15 to 3:15

**Parse Error**
at 3:5 to 3:18

**Parse Error**
at 1:13 to 3:30

**Parse Error**
at 1:1 to 3:30

**Parse Error**
at 3:30 to 3:30

**Unsupported Node**
at 1:13 to 3:32

**Unsupported Node**
at 3:30 to 3:48

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
