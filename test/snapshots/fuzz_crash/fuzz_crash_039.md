# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[}('
)
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseCurly OpenRound MalformedSingleQuoteUnclosed CloseRound ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (malformed)
))
~~~
# FORMATTED
~~~roc
module [}]'
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **exposed_item_unexpected_token**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_039.md:1:8:1:9:**
```roc
module[}('
```
       ^


**PARSE ERROR**
A parsing error occurred: **header_expected_close_square**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_039.md:1:1:1:9:**
```roc
module[}('
```
^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **'
** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_039.md:1:10:2:1:**
```roc
module[}('
)
```


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_039.md:1:10:2:1:**
```roc
module[}('
)
```


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
