# META
~~~ini
description=Match expression with guard conditions using if clauses
type=expr
~~~
# SOURCE
~~~roc
match value {
    x if x > 0 => "positive: ${Num.toStr x}"
    x if x < 0 => "negative: ${Num.toStr x}"
    _ => "other"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly LowerIdent KwIf LowerIdent OpGreaterThan Int OpFatArrow String LowerIdent KwIf LowerIdent OpLessThan Int OpFatArrow String Underscore OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match <17 branches>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - guards_1.md:2:7:2:7
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:2:16:2:18
IF WITHOUT ELSE - guards_1.md:2:7:2:9
UNEXPECTED TOKEN IN PATTERN - guards_1.md:2:20:2:30
PARSE ERROR - guards_1.md:2:30:2:30
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:2:30:2:32
UNEXPECTED TOKEN IN PATTERN - guards_1.md:2:32:2:35
PARSE ERROR - guards_1.md:2:43:2:43
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:2:43:2:44
UNEXPECTED TOKEN IN PATTERN - guards_1.md:2:44:2:44
PARSE ERROR - guards_1.md:2:44:2:44
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:2:44:2:45
PARSE ERROR - guards_1.md:3:7:3:7
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:3:16:3:18
IF WITHOUT ELSE - guards_1.md:3:7:3:9
UNEXPECTED TOKEN IN PATTERN - guards_1.md:3:20:3:30
PARSE ERROR - guards_1.md:3:30:3:30
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:3:30:3:32
UNEXPECTED TOKEN IN PATTERN - guards_1.md:3:32:3:35
PARSE ERROR - guards_1.md:3:43:3:43
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:3:43:3:44
UNEXPECTED TOKEN IN PATTERN - guards_1.md:3:44:3:44
PARSE ERROR - guards_1.md:3:44:3:44
UNEXPECTED TOKEN IN EXPRESSION - guards_1.md:3:44:3:45
UNDEFINED VARIABLE - guards_1.md:1:7:1:12
UNRECOGNIZED SYNTAX - guards_1.md:2:7:2:20
UNUSED VARIABLE - guards_1.md:2:5:2:6
# PROBLEMS
**Parse Error**
at 1:1 to 1:13

**Parse Error**
at 2:7 to 3:5

**Parse Error**
at 3:7 to 4:5

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
