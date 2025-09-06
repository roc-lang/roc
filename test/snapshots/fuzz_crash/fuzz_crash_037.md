# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]"\
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare MalformedString ~~~
# PARSE
~~~clojure
(module-header)
(block
  (malformed)
)
~~~
# FORMATTED
~~~roc
module []

"\
~~~
# EXPECTED
INVALID ESCAPE SEQUENCE - :0:0:0:0
UNCLOSED STRING - :0:0:0:0
PARSE ERROR - fuzz_crash_037.md:1:9:1:10
PARSE ERROR - fuzz_crash_037.md:1:10:1:11
PARSE ERROR - fuzz_crash_037.md:1:11:1:11
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **"\** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_037.md:1:9:1:11:**
```roc
module[]"\
```
        ^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 4
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
~~~
# TYPES
~~~roc
~~~
