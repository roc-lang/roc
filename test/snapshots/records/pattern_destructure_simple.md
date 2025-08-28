# META
~~~ini
description=Simple record destructuring pattern
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name, age } => name
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenCurly LowerIdent Comma LowerIdent CloseCurly OpFatArrow LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (lc "person")
))
~~~
# FORMATTED
~~~roc
match person
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:19 to 2:19

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
