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
(binop_thick_arrow
  (match <0 branches>)
  (ellipsis)
)
~~~
# FORMATTED
~~~roc
match numbers
 => ... # error, multiple rest patterns not allowed
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:8 to 2:10

**Parse Error**
at 2:5 to 2:10

**Parse Error**
at 1:15 to 2:20

**Parse Error**
at 1:15 to 2:22

**Unsupported Node**
at 2:22 to 2:24

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
