# META
~~~ini
description=Match expression with underscore patterns in list matching
type=expr
~~~
# SOURCE
~~~roc
match items {
    [_] => 1 # pattern match on a list with a single (ignored) element
    [.., last] => last # pattern match on the last item in the list
    [first, ..] => first # pattern match on the first item in the list
    [_, _, third] => third # pattern match on the third item in the list
    [x, _, _, y] => x + y # first + fourth item in the list
    [] => 0 # match an empty list
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare Underscore CloseSquare OpFatArrow Int OpenSquare DoubleDot Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare LowerIdent Comma DoubleDot CloseSquare OpFatArrow LowerIdent OpenSquare Underscore Comma Underscore Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare LowerIdent Comma Underscore Comma Underscore Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent OpenSquare CloseSquare OpFatArrow Int CloseCurly ~~~
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
at 3:8 to 3:10

**Parse Error**
at 3:5 to 3:10

**Parse Error**
at 3:5 to 3:14

**Parse Error**
at 3:14 to 3:16

**Unsupported Node**
at 3:14 to 3:16

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
