# META
~~~ini
description=Match expression with multiple patterns in one branch
type=expr
~~~
# SOURCE
~~~roc
match color {
    Blue | Green | Red => 1
    Black => 2
    White => 3
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpBar UpperIdent OpBar UpperIdent OpFatArrow Int UpperIdent OpFatArrow Int UpperIdent OpFatArrow Int CloseCurly ~~~
# PARSE
~~~clojure
(match <13 branches>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNDEFINED VARIABLE - multi_pattern_branch.md:1:7:1:12
# PROBLEMS
**Parse Error**
at 1:1 to 1:13

**Parse Error**
at 2:24 to 2:24

**Parse Error**
at 3:11 to 3:11

**Parse Error**
at 4:11 to 4:11

**Parse Error**
at 1:1 to 5:2

**Parse Error**
at 5:2 to 5:2

**Unsupported Node**
at 1:13 to 5:1

**Unsupported Node**
at 5:2 to 5:2

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
