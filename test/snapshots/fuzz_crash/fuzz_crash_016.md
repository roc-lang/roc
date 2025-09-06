# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0|
~~~
# TOKENS
~~~text
Int OpBar ~~~
# PARSE
~~~clojure
(block
  (num_literal_i32 0)
  (malformed)
)
~~~
# FORMATTED
~~~roc
0
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_016.md:1:1:1:2
PARSE ERROR - fuzz_crash_016.md:1:2:1:3
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_expr_bar**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_016.md:1:2:1:3:**
```roc
0|
```
 ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.



# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.num_literal_i32 0)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 5
(var #0 _)
(var #1 Num *)
(var #2 _)
(var #3 _)
(var #4 _)
~~~
# TYPES
~~~roc
~~~
