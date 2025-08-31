# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[}{0      0)(0}
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseCurly OpenCurly Int Int CloseRound OpenRound Int CloseCurly ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (malformed malformed:exposed_item_unexpected_token)
))
~~~
# FORMATTED
~~~roc
module [}]

{
	0
	0
	)(0)
}
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **exposed_item_unexpected_token**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_051.md:1:8:1:9:**
```roc
module[}{0      0)(0}
```
       ^


**PARSE ERROR**
A parsing error occurred: **header_expected_close_square**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_051.md:1:1:1:9:**
```roc
module[}{0      0)(0}
```
^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **)** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_051.md:1:18:1:19:**
```roc
module[}{0      0)(0}
```
                 ^


**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_051.md:1:18:1:21:**
```roc
module[}{0      0)(0}
```
                 ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_051.md:1:9:1:22:**
```roc
module[}{0      0)(0}
```
        ^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
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
