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
(match
  (scrutinee     (lc "numbers")
))
~~~
# FORMATTED
~~~roc
match numbers
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:8 to 2:8

**Parse Error**
at 2:5 to 2:10

**Parse Error**
at 1:15 to 2:20

# CANONICALIZE
~~~clojure
(Expr.dot_num)
~~~
# SOLVED
~~~clojure
(expr :tag dot_num :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
