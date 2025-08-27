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
at 2:16 to 2:16

**Parse Error**
at 2:5 to 2:19

**Parse Error**
at 2:27 to 2:27

**Parse Error**
at 2:29 to 2:29

**Parse Error**
at 3:8 to 3:8

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
