# META
~~~ini
description=Let-polymorphism with empty list in expression
type=expr
~~~
# SOURCE
~~~roc
match [] {
    empty => { ints: [1, 2, 3], strs: ["a", "b", "c"], empty: empty }
}
~~~
# TOKENS
~~~text
KwMatch OpenSquare CloseSquare OpenCurly LowerIdent OpFatArrow OpenCurly LowerIdent OpColon OpenSquare Int Comma Int Comma Int CloseSquare Comma LowerIdent OpColon OpenSquare String Comma String Comma String CloseSquare Comma LowerIdent OpColon LowerIdent CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (list_literal)
))
~~~
# FORMATTED
~~~roc
match []
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:11 to 2:11

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
