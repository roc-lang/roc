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
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
..
~~~
# EXPECTED
NIL
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


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**empty_list_before_rest_pattern.md:3:6:3:8:**
```roc
    [.., e] => Ok(e)
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
