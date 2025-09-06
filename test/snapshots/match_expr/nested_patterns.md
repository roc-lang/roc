# META
~~~ini
description=Match expression with nested patterns (tags containing records, lists with tags)
type=expr
~~~
# SOURCE
~~~roc
match data {
    Container({ items: [First(x), .. as rest] }) => x + List.len(rest)
    Container({ items: [] }) => 0
    Wrapper([Tag(value), Other(y)]) => value + y
    Simple(x) => x
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly UpperIdent OpenRound OpenCurly LowerIdent OpColon OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma DoubleDot KwAs LowerIdent CloseSquare CloseCurly CloseRound OpFatArrow LowerIdent OpPlus UpperIdent Dot LowerIdent OpenRound LowerIdent CloseRound UpperIdent OpenRound OpenCurly LowerIdent OpColon OpenSquare CloseSquare CloseCurly CloseRound OpFatArrow Int UpperIdent OpenRound OpenSquare UpperIdent OpenRound LowerIdent CloseRound Comma UpperIdent OpenRound LowerIdent CloseRound CloseSquare CloseRound OpFatArrow LowerIdent OpPlus LowerIdent UpperIdent OpenRound LowerIdent CloseRound OpFatArrow LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
) 
~~~
# EXPECTED
UNDEFINED VARIABLE - nested_patterns.md:1:7:1:11
UNDEFINED VARIABLE - nested_patterns.md:2:57:2:65
# PROBLEMS
**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**nested_patterns.md:2:38:2:41:**
```roc
    Container({ items: [First(x), .. as rest] }) => x + List.len(rest)
```
                                     ^^^


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**nested_patterns.md:2:41:2:45:**
```roc
    Container({ items: [First(x), .. as rest] }) => x + List.len(rest)
```
                                        ^^^^


**PARSE ERROR**
A parsing error occurred: **expected_close_round**
This is an unexpected parsing error. Please check your syntax.

**nested_patterns.md:2:45:2:47:**
```roc
    Container({ items: [First(x), .. as rest] }) => x + List.len(rest)
```
                                            ^^


**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**nested_patterns.md:2:47:2:48:**
```roc
    Container({ items: [First(x), .. as rest] }) => x + List.len(rest)
```
                                              ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **) ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**nested_patterns.md:2:48:2:50:**
```roc
    Container({ items: [First(x), .. as rest] }) => x + List.len(rest)
```
                                               ^^


# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
; Total type variables: 19
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
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 _)
(var #17 _)
(var #18 _)
~~~
# TYPES
~~~roc
~~~
