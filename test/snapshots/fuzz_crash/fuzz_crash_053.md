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
    (malformed malformed:exposed_item_unexpected_token)
))
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
NIL
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


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_053.md:1:9:1:13:**
```roc
module[){..0,)
```
        ^^^^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_053.md:1:12:1:13:**
```roc
module[){..0,)
```
           ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_053.md:1:13:1:14:**
```roc
module[){..0,)
```
            ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_053.md:1:14:1:15:**
```roc
module[){..0,)
```
             ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
