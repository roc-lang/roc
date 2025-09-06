# META
~~~ini
description=Match expression with wrong arrow
type=expr
~~~
# SOURCE
~~~roc
match l {
    [] -> Err(EmptyList)
    [.., e] -> Ok(e)
}
~~~
# TOKENS
~~~text
KwMatch LowerIdent OpenCurly OpenSquare CloseSquare OpArrow UpperIdent OpenRound UpperIdent CloseRound OpenSquare DoubleDot Comma LowerIdent CloseSquare OpArrow UpperIdent OpenRound LowerIdent CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
EmptyList
~~~
# EXPECTED
PARSE ERROR - wrong_arrow.md:2:8:2:8
PARSE ERROR - wrong_arrow.md:3:13:3:13
UNDEFINED VARIABLE - wrong_arrow.md:1:7:1:8
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **Err** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**wrong_arrow.md:2:11:2:14:**
```roc
    [] -> Err(EmptyList)
```
          ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **[] -> Err(** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**wrong_arrow.md:2:5:2:15:**
```roc
    [] -> Err(EmptyList)
```
    ^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **EmptyList** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**wrong_arrow.md:2:15:2:24:**
```roc
    [] -> Err(EmptyList)
```
              ^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
; Total type variables: 9
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
~~~
# TYPES
~~~roc
~~~
