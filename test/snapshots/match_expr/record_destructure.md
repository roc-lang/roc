# META
~~~ini
description=Match expression with record destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match ... {
    { name, age } => "${name} is ${age.to_str()} years old"
    { name, address: { city } } => "${city} is the city of ${name}"
    {} => "empty"
}
~~~
# TOKENS
~~~text
KwMatch TripleDot OpenCurly OpenCurly LowerIdent Comma LowerIdent CloseCurly OpFatArrow String OpenCurly LowerIdent Comma LowerIdent OpColon OpenCurly LowerIdent CloseCurly CloseCurly OpFatArrow String OpenCurly CloseCurly OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (ellipsis)
))
~~~
# FORMATTED
~~~roc
match ...
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:19 to 2:19

**Parse Error**
at 3:33 to 3:33

**Parse Error**
at 4:8 to 4:8

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
