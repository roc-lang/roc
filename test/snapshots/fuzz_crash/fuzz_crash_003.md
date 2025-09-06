# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
= "te
~~~
# TOKENS
~~~text
OpAssign MalformedString ~~~
# PARSE
~~~clojure
(block
  (malformed)
  (malformed)
)
~~~
# FORMATTED
~~~roc
"te
~~~
# EXPECTED
UNCLOSED STRING - :0:0:0:0
MISSING HEADER - fuzz_crash_003.md:1:1:1:2
PARSE ERROR - fuzz_crash_003.md:1:3:1:4
PARSE ERROR - fuzz_crash_003.md:1:4:1:6
PARSE ERROR - fuzz_crash_003.md:1:6:1:6
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token ** ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_003.md:1:2:1:3:**
```roc
= "te
```
 ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"te** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_003.md:1:3:1:6:**
```roc
= "te
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
; Total type variables: 6
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
~~~
# TYPES
~~~roc
~~~
