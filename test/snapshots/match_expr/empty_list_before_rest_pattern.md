# META
~~~ini
description=Match expression with empty list pattern followed by list rest pattern (segfault regression test)
type=expr
~~~
# SOURCE
~~~roc
match l {
    [] => Err(EmptyList)
    [.., e] => Ok(e)
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare CloseSquare OpFatArrow UpperIdent OpenRound UpperIdent CloseRound OpenSquare DoubleDot Comma LowerIdent CloseSquare OpFatArrow UpperIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
..
~~~
# EXPECTED
UNDEFINED VARIABLE - empty_list_before_rest_pattern.md:1:7:1:8
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **Err** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**empty_list_before_rest_pattern.md:2:11:2:14:**
```roc
    [] => Err(EmptyList)
```
          ^^^


**PARSE ERROR**
A parsing error occurred: **expected_arrow_after_pattern**
This is an unexpected parsing error. Please check your syntax.

**empty_list_before_rest_pattern.md:3:5:3:6:**
```roc
    [.., e] => Ok(e)
```
    ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **..** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**empty_list_before_rest_pattern.md:3:6:3:8:**
```roc
    [.., e] => Ok(e)
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
