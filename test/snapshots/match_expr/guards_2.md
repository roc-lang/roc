# META
~~~ini
description=Match expression with guard conditions using if clauses
type=expr
~~~
# SOURCE
~~~roc
match value {
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
    [x, y] if x == y => "pair of equal values: ${Num.toStr x}"
    _ => "other"
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare LowerIdent Comma DoubleDot KwAs LowerIdent CloseSquare KwIf UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound OpGreaterThan Int OpFatArrow String OpenSquare LowerIdent Comma LowerIdent CloseSquare KwIf LowerIdent OpEquals LowerIdent OpFatArrow String Underscore OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(match <27 branches>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - guards_2.md:2:25:2:25
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:2:47:2:49
IF WITHOUT ELSE - guards_2.md:2:25:2:27
UNEXPECTED TOKEN IN PATTERN - guards_2.md:2:51:2:75
PARSE ERROR - guards_2.md:2:75:2:75
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:2:75:2:77
UNEXPECTED TOKEN IN PATTERN - guards_2.md:2:77:2:80
PARSE ERROR - guards_2.md:2:92:2:92
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:2:92:2:93
UNEXPECTED TOKEN IN PATTERN - guards_2.md:2:93:2:93
PARSE ERROR - guards_2.md:2:93:2:93
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:2:93:2:94
PARSE ERROR - guards_2.md:3:12:3:12
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:3:22:3:24
IF WITHOUT ELSE - guards_2.md:3:12:3:14
UNEXPECTED TOKEN IN PATTERN - guards_2.md:3:26:3:48
PARSE ERROR - guards_2.md:3:48:3:48
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:3:48:3:50
UNEXPECTED TOKEN IN PATTERN - guards_2.md:3:50:3:53
PARSE ERROR - guards_2.md:3:61:3:61
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:3:61:3:62
UNEXPECTED TOKEN IN PATTERN - guards_2.md:3:62:3:62
PARSE ERROR - guards_2.md:3:62:3:62
UNEXPECTED TOKEN IN EXPRESSION - guards_2.md:3:62:3:63
UNDEFINED VARIABLE - guards_2.md:1:7:1:12
UNRECOGNIZED SYNTAX - guards_2.md:2:25:2:51
UNUSED VARIABLE - guards_2.md:2:6:2:11
UNUSED VARIABLE - guards_2.md:1:1:1:1
# PROBLEMS
**Parse Error**
at 1:1 to 1:13

**Parse Error**
at 2:16 to 2:16

**Parse Error**
at 2:5 to 2:19

**Parse Error**
at 2:23 to 2:23

**Parse Error**
at 2:25 to 3:5

**Parse Error**
at 3:12 to 4:5

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
