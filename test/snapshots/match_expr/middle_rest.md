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
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
] 
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:15 to 2:17

**Parse Error**
at 2:5 to 2:17

**Parse Error**
at 2:5 to 2:21

**Parse Error**
at 2:21 to 2:23

**Unsupported Node**
at 2:21 to 2:23

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
