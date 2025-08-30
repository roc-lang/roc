# META
~~~ini
description=Match expression with various list destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match list {
    [] => 0
    [x] => x
    [first, second] => first + second
    [head, .. as tail] => head
    [One, Two, .. as rest] => 3
    [x, y, z, .. as more] => x + y + z
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare CloseSquare OpFatArrow Int OpenSquare LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent OpenSquare LowerIdent Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare UpperIdent Comma UpperIdent Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow Int OpenSquare LowerIdent Comma LowerIdent Comma LowerIdent Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent OpPlus LowerIdent CloseCurly ~~~
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
at 5:15 to 5:18

**Parse Error**
at 5:5 to 5:18

**Parse Error**
at 5:5 to 5:22

**Parse Error**
at 5:22 to 5:24

**Unsupported Node**
at 5:22 to 5:24

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
