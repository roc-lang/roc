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
(block
  (malformed)
)
~~~
# FORMATTED
~~~roc
module [}]

'
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_039.md:1:8:1:9
PARSE ERROR - fuzz_crash_039.md:3:1:3:1
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


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 5
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
~~~
# TYPES
~~~roc
~~~
