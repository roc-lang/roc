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
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
EmptyList
~~~
# EXPECTED
NIL
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
(expr :tag malformed :type "Error")
~~~
# TYPES
~~~roc
Error
~~~
