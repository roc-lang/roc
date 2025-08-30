# META
~~~ini
description=Match expression with f64 literal pattern (should error)
type=expr
~~~
# SOURCE
~~~roc
match x {
    3.14f64 => "pi"
    0.0f64 => "zero"
    value => "other"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly Float LowerIdent OpFatArrow String Float LowerIdent OpFatArrow String LowerIdent OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
0.0
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:5 to 2:13

**Parse Error**
at 3:5 to 3:8

**Unsupported Node**
at 3:5 to 3:8

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
