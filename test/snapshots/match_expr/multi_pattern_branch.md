# META
~~~ini
description=Match expression with multiple patterns in one branch
type=expr
~~~
# SOURCE
~~~roc
match color {
    Blue | Green | Red => 1
    Black => 2
    White => 3
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpBar UpperIdent OpBar UpperIdent OpFatArrow Int UpperIdent OpFatArrow Int UpperIdent OpFatArrow Int CloseCurly ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
Green 
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:5 to 2:12

**Parse Error**
at 2:12 to 2:18

**Unsupported Node**
at 2:12 to 2:18

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
