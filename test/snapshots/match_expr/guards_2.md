# META
~~~ini
description=Match expression with guard conditions using if clauses
type=expr
~~~
# SOURCE
~~~roc
match value {
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
    _ => "other"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare LowerIdent Comma DoubleDot KwAs LowerIdent CloseSquare KwIf UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound OpGreaterThan Int OpFatArrow String OpenSquare LowerIdent Comma LowerIdent CloseSquare KwIf LowerIdent OpEquals LowerIdent OpFatArrow String Underscore OpFatArrow String CloseCurly ~~~
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
at 2:5 to 2:23

**Parse Error**
at 2:23 to 2:25

**Unsupported Node**
at 2:23 to 2:25

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
