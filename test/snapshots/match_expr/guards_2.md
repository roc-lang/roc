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
KwMatch LowerIdent OpenCurly OpenSquare LowerIdent Comma DoubleDot KwAs LowerIdent CloseSquare KwIf UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound OpGreaterThan Int OpThinArrow String OpenSquare LowerIdent Comma LowerIdent CloseSquare KwIf LowerIdent OpEquals LowerIdent OpThinArrow String Underscore OpThinArrow String CloseCurly ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
]
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
**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**guards_2.md:2:16:2:19:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
               ^^^


**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**guards_2.md:2:19:2:23:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                  ^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**guards_2.md:2:23:2:25:**
```roc
    [first, .. as rest] if List.len(rest) > 5 => "long list starting with ${Num.toStr first}"
```
                      ^^


# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
; Total type variables: 10
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
~~~
# TYPES
~~~roc
~~~
