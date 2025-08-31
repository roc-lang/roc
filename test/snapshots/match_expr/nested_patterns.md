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
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
) 
~~~
# EXPECTED
NIL
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


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**nested_patterns.md:2:48:2:50:**
```roc
    Container({ items: [First(x), .. as rest] }) => x + List.len(rest)
```
                                               ^^


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
