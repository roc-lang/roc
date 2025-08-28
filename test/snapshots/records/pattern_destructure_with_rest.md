# META
~~~ini
description=Record destructuring with rest pattern
type=expr
~~~
# SOURCE
~~~roc
match person {
    { first_name, ..others } => Str.len(first_name) > Str.len(others.last_name)
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenCurly LowerIdent Comma DoubleDot LowerIdent CloseCurly OpFatArrow UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound OpGreaterThan UpperIdent Dot LowerIdent OpenRound LowerIdent Dot LowerIdent CloseRound CloseCurly ~~~
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
at 2:30 to 2:30

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
