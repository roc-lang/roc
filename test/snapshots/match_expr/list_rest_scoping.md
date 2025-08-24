# META
~~~ini
description=Match expression with list rest patterns testing variable scoping
type=expr
~~~
# SOURCE
~~~roc
match items {
    [first, ..rest] => first + 1
    [..rest, last] => last + 2
    [x, ..rest, y] => x + y
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare LowerIdent Comma DoubleDot LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus Int OpenSquare DoubleDot LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus Int OpenSquare LowerIdent Comma DoubleDot LowerIdent Comma LowerIdent CloseSquare OpFatArrow LowerIdent OpPlus LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match <22 branches>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
BAD LIST REST PATTERN SYNTAX - list_rest_scoping.md:2:13:2:19
BAD LIST REST PATTERN SYNTAX - list_rest_scoping.md:3:6:3:12
BAD LIST REST PATTERN SYNTAX - list_rest_scoping.md:4:9:4:15
UNDEFINED VARIABLE - list_rest_scoping.md:1:7:1:12
UNUSED VARIABLE - list_rest_scoping.md:2:15:2:15
UNUSED VARIABLE - list_rest_scoping.md:3:8:3:8
UNUSED VARIABLE - list_rest_scoping.md:4:11:4:11
# PROBLEMS
**Parse Error**
at 1:1 to 1:13

**Parse Error**
at 2:21 to 2:21

**Parse Error**
at 3:20 to 3:20

**Parse Error**
at 4:20 to 4:20

**Parse Error**
at 1:1 to 5:2

**Parse Error**
at 5:2 to 5:2

**Unsupported Node**
at 1:1 to 5:2

# CANONICALIZE
~~~clojure
(Stmt.malformed)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# No expression found
~~~
