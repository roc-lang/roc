# META
~~~ini
description=List rest patterns should have correct list types matching element types
type=expr
~~~
# SOURCE
~~~roc
match numbers {
    [first, .. as restNums] => restNums
    [] => []
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare LowerIdent Comma DoubleDot KwAs LowerIdent CloseSquare OpFatArrow LowerIdent OpenSquare CloseSquare OpFatArrow OpenSquare CloseSquare CloseCurly ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
] 
~~~
# EXPECTED
UNDEFINED VARIABLE - can_list_rest_types.md:1:7:1:14
UNUSED VARIABLE - can_list_rest_types.md:2:6:2:11
# PROBLEMS
**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**can_list_rest_types.md:2:16:2:19:**
```roc
    [first, .. as restNums] => restNums
```
               ^^^


**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**can_list_rest_types.md:2:19:2:27:**
```roc
    [first, .. as restNums] => restNums
```
                  ^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **] ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_list_rest_types.md:2:27:2:29:**
```roc
    [first, .. as restNums] => restNums
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
