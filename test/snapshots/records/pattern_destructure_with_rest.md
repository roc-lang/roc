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
(match <17 branches>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNDEFINED VARIABLE - pattern_destructure_with_rest.md:1:7:1:13
UNDEFINED VARIABLE - pattern_destructure_with_rest.md:2:33:2:40
UNDEFINED VARIABLE - pattern_destructure_with_rest.md:2:55:2:62
# PROBLEMS
**Parse Error**
at 1:1 to 1:14

**Parse Error**
at 2:30 to 2:30

**Parse Error**
at 1:1 to 3:2

**Parse Error**
at 3:2 to 3:2

**Unsupported Node**
at 1:14 to 3:1

**Unsupported Node**
at 3:2 to 3:2

# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
(expr :tag match :type "Error")
~~~
# TYPES
~~~roc
Error
~~~
