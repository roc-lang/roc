# META
~~~ini
description=Match expression with deeply nested record patterns
type=expr
~~~
# SOURCE
~~~roc
match ... {
    { name, address: { city, country } } => "${name} lives in ${city}, ${country}"
    { person: { name, age }, location: { city } } => "${name} (${age.to_str()}) from ${city}"
    { data: { info: { value } } } => "Deep nested: ${value}"
    { simple } => "Simple: ${simple}"
    {} => "empty"
}
~~~
# TOKENS
~~~text
KwMatch TripleDot OpenCurly OpenCurly LowerIdent Comma LowerIdent OpColon OpenCurly LowerIdent Comma LowerIdent CloseCurly CloseCurly OpFatArrow String OpenCurly LowerIdent OpColon OpenCurly LowerIdent Comma LowerIdent CloseCurly Comma LowerIdent OpColon OpenCurly LowerIdent CloseCurly CloseCurly OpFatArrow String OpenCurly LowerIdent OpColon OpenCurly LowerIdent OpColon OpenCurly LowerIdent CloseCurly CloseCurly CloseCurly OpFatArrow String OpenCurly LowerIdent CloseCurly OpFatArrow String OpenCurly CloseCurly OpFatArrow String CloseCurly ~~~
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
at 2:42 to 2:42

**Parse Error**
at 3:51 to 3:51

**Parse Error**
at 4:35 to 4:35

**Parse Error**
at 5:16 to 5:16

**Parse Error**
at 6:8 to 6:8

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
