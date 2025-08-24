# META
~~~ini
description=Match expression with list patterns including invalid rest pattern
type=expr
~~~
# SOURCE
~~~roc
match numbers {
    [] => acc
    [first, ..rest] => 0 # invalid rest pattern should error
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare CloseSquare OpFatArrow LowerIdent OpenSquare LowerIdent Comma DoubleDot LowerIdent CloseSquare OpFatArrow Int CloseCurly ~~~
# PARSE
~~~clojure
(match <8 branches>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
BAD LIST REST PATTERN SYNTAX - list_patterns.md:3:13:3:19
UNDEFINED VARIABLE - list_patterns.md:1:7:1:14
UNDEFINED VARIABLE - list_patterns.md:2:11:2:14
UNUSED VARIABLE - list_patterns.md:3:6:3:11
UNUSED VARIABLE - list_patterns.md:3:15:3:15
# PROBLEMS
**Parse Error**
at 1:1 to 1:15

**Parse Error**
at 2:8 to 2:8

**Parse Error**
at 3:21 to 3:21

**Parse Error**
at 1:1 to 4:2

**Parse Error**
at 4:2 to 4:2

**Unsupported Node**
at 1:15 to 3:26

**Unsupported Node**
at 4:2 to 4:2

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
