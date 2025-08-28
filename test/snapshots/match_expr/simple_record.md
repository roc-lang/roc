# META
~~~ini
description=Simple record destructuring in match expression
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name } => name
    { age } => age
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenCurly LowerIdent CloseCurly OpFatArrow LowerIdent OpenCurly LowerIdent CloseCurly OpFatArrow LowerIdent CloseCurly ~~~
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
at 2:14 to 2:14

**Parse Error**
at 3:13 to 3:13

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
