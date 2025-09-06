# META
~~~ini
description=Type mismatch with instantiated function arguments
type=expr
~~~
# SOURCE
~~~roc
{
    pair : a, a -> (a, a)
    pair = |x, y| (x, y)

    pair(42, "hello")
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpenRound Int Comma String CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
pair 
~~~
# EXPECTED
TYPE MISMATCH - test_instantiated_arg_mismatch.md:5:10:5:10
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**test_instantiated_arg_mismatch.md:1:1:2:17:**
```roc
{
    pair : a, a -> (a, a)
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **pair ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**test_instantiated_arg_mismatch.md:3:5:3:10:**
```roc
    pair = |x, y| (x, y)
```
    ^^^^^


# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
; Total type variables: 12
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
~~~
# TYPES
~~~roc
~~~
