# META
~~~ini
description=List rest patterns should have correct list types matching element types
type=expr
~~~
# SOURCE
~~~roc
match numbers {
    [first, .. as restNums] => restNums
    [] => []
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare LowerIdent Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare CloseSquare OpFatArrow OpenSquare CloseSquare CloseCurly ~~~
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
at 2:16 to 2:19

**Parse Error**
at 2:5 to 2:19

**Parse Error**
at 2:5 to 2:27

**Parse Error**
at 2:27 to 2:29

**Unsupported Node**
at 2:27 to 2:29

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
