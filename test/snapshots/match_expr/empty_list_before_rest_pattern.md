# META
~~~ini
description=Match expression with empty list pattern followed by list rest pattern (segfault regression test)
type=expr
~~~
# SOURCE
~~~roc
match l {
    [] => Err(EmptyList)
    [.., e] => Ok(e)
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare CloseSquare OpFatArrow UpperIdent OpenRound UpperIdent CloseRound OpenSquare DoubleDot Comma LowerIdent CloseSquare OpFatArrow UpperIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
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
at 3:5 to 3:11

**Parse Error**
at 3:11 to 3:13

**Unsupported Node**
at 3:11 to 3:13

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
