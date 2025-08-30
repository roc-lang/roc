# META
~~~ini
description=Match expression with guard conditions using if clauses
type=expr
~~~
# SOURCE
~~~roc
match value {
    x if x > 0 => "positive: ${Num.toStr x}"
    x if x < 0 => "negative: ${Num.toStr x}"
    _ => "other"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly LowerIdent KwIf LowerIdent OpGreaterThan Int OpFatArrow String LowerIdent KwIf LowerIdent OpLessThan Int OpFatArrow String Underscore OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
x 
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:5 to 2:10

**Parse Error**
at 2:10 to 2:12

**Unsupported Node**
at 2:10 to 2:12

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
