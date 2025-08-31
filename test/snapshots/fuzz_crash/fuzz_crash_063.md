# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[}0}.a
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseCurly Int CloseCurly Dot LowerIdent ~~~
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

0
} | .a
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **exposed_item_unexpected_token**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_063.md:1:8:1:9:**
```roc
module[}0}.a
```
       ^


**PARSE ERROR**
A parsing error occurred: **header_expected_close_square**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_063.md:1:1:1:9:**
```roc
module[}0}.a
```
^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_063.md:1:10:1:11:**
```roc
module[}0}.a
```
         ^


**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_063.md:1:9:1:10:**
```roc
module[}0}.a
```
        ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_063.md:1:10:1:13:**
```roc
module[}0}.a
```
         ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~
