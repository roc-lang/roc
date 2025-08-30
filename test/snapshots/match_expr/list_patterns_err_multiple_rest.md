# META
~~~ini
description=Match expression with more than one rest pattern not permitted, should error
type=expr
~~~
# SOURCE
~~~roc
match numbers {
    [.., middle, ..] => ... # error, multiple rest patterns not allowed
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare DoubleDot Comma LowerIdent Comma DoubleDot CloseSquare OpFatArrow TripleDot CloseCurly ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:8 to 2:10

**Parse Error**
at 2:5 to 2:10

**Parse Error**
at 2:5 to 2:16

**Parse Error**
at 2:20 to 2:22

**Parse Error**
at 3:1 to 3:2

**Unsupported Node**
at 3:1 to 3:2

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
