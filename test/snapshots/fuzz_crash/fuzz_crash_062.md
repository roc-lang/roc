# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[}|0
as s|||0
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseCurly OpBar Int KwAs LowerIdent OpOr OpBar Int ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (malformed)
))
~~~
# FORMATTED
~~~roc
module [}]as 
s || 
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **exposed_item_unexpected_token**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_062.md:1:8:1:9:**
```roc
module[}|0
```
       ^


**PARSE ERROR**
A parsing error occurred: **header_expected_close_square**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_062.md:1:1:1:9:**
```roc
module[}|0
```
^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **as ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_062.md:2:1:2:4:**
```roc
as s|||0
```
^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.



**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_062.md:2:1:2:4:**
```roc
as s|||0
```
^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_062.md:2:4:2:9:**
```roc
as s|||0
```
   ^^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
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
