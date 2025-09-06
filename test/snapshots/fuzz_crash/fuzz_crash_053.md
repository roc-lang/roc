# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[){..0,)
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseRound OpenCurly DoubleDot Int Comma CloseRound ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (malformed)
))
(block
  (record_literal
    (underscore)
  )
  (num_literal_i32 0)
  (malformed)
  (malformed)
)
~~~
# FORMATTED
~~~roc
module [)]

{ _ }
0
,
)
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_053.md:1:8:1:9
PARSE ERROR - fuzz_crash_053.md:2:1:2:1
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **exposed_item_unexpected_token**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_053.md:1:8:1:9:**
```roc
module[){..0,)
```
       ^


**PARSE ERROR**
A parsing error occurred: **header_expected_close_square**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_053.md:1:1:1:9:**
```roc
module[){..0,)
```
^^^^^^^^


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_053.md:1:9:1:12:**
```roc
module[){..0,)
```
        ^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **,** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_053.md:1:13:1:14:**
```roc
module[){..0,)
```
            ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_053.md:1:14:1:15:**
```roc
module[){..0,)
```
             ^


**PATTERN IN EXPRESSION CONTEXT**
Found a pattern where an expression was expected.
Patterns can only appear in specific contexts like function parameters, destructuring assignments, or **when** branches.



# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.record_literal
    (Expr.malformed)
  )
  (Expr.num_literal_i32 0)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 11
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 -> #8)
(var #4 Num *)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 {})
(var #9 _)
(var #10 _)
~~~
# TYPES
~~~roc
~~~
