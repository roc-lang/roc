# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
mo|%
~~~
# TOKENS
~~~text
LowerIdent OpBar MalformedUnknownToken ~~~
# PARSE
~~~clojure
(block
  (lc "mo")
  (malformed)
)
~~~
# FORMATTED
~~~roc
mo
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_001.md:1:1:1:3
PARSE ERROR - fuzz_crash_001.md:1:3:1:4
PARSE ERROR - fuzz_crash_001.md:1:4:1:5
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **%** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_001.md:1:4:1:5:**
```roc
mo|%
```
   ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.



**UNDEFINED VARIABLE**
Nothing is named **mo** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_001.md:1:1:1:3:**
```roc
mo|%
```
^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.lookup "mo")
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
