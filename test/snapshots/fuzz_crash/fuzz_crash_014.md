# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0b.0
0bu22
0u22
~~~
# TOKENS
~~~text
MalformedNumberNoDigits Dot Int MalformedNumberNoDigits LowerIdent Int LowerIdent ~~~
# PARSE
~~~clojure
(block
  (binop_pipe
    (malformed)
    (num_literal_i32 0)
  )
  (malformed)
  (lc "u22")
  (num_literal_i32 0)
  (lc "u22")
)
~~~
# FORMATTED
~~~roc
 | 0
0b
u22
0
u22
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_014.md:1:1:1:3
PARSE ERROR - fuzz_crash_014.md:1:3:1:5
PARSE ERROR - fuzz_crash_014.md:2:1:2:6
PARSE ERROR - fuzz_crash_014.md:3:1:3:5
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.



**UNEXPECTED TOKEN IN EXPRESSION**
The token **0b** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_014.md:2:1:2:3:**
```roc
0bu22
```
^^


**UNDEFINED VARIABLE**
Nothing is named **u22** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_014.md:2:3:2:6:**
```roc
0bu22
```
  ^^^


**UNDEFINED VARIABLE**
Nothing is named **u22** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_014.md:3:2:3:5:**
```roc
0u22
```
 ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_pipe)
  (Expr.malformed)
  (Expr.lookup "u22")
  (Expr.num_literal_i32 0)
  (Expr.lookup "u22")
)
~~~
# SOLVED
~~~clojure
; Total type variables: 10
(var #0 _)
(var #1 _)
(var #2 Num *)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 Num *)
(var #7 _)
(var #8 _)
(var #9 _)
~~~
# TYPES
~~~roc
~~~
