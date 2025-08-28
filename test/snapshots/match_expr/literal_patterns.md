# META
~~~ini
description=Match expression with tag patterns for different cases
type=expr
~~~
# SOURCE
~~~roc
match Answer {
    Answer => 1
    Zero => "hello"
    Greeting => 3
    10 => 4
}
~~~
# TOKENS
~~~text
KwMatch UpperIdent OpenCurly UpperIdent OpFatArrow Int UpperIdent OpFatArrow String UpperIdent OpFatArrow Int Int OpFatArrow Int CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (uc "Answer")
))
~~~
# FORMATTED
~~~roc
match Answer
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:12 to 2:12

**Parse Error**
at 3:10 to 3:10

**Parse Error**
at 4:14 to 4:14

**Parse Error**
at 5:8 to 5:8

# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
(expr :tag match :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
