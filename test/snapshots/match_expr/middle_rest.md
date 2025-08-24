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
NO CHANGE
~~~
# EXPECTED
UNDEFINED VARIABLE - middle_rest.md:1:7:1:12
UNUSED VARIABLE - middle_rest.md:1:1:1:1
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
at 1:1 to 3:48

# CANONICALIZE
~~~clojure
(Stmt.malformed)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# No expression found
~~~
