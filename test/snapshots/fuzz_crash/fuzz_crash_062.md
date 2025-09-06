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
(block
  (malformed)
  (binop_or
    (lc "s")
    (malformed)
  )
)
~~~
# FORMATTED
~~~roc
module [}]

as 
s || 
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_062.md:1:8:1:9
PARSE ERROR - fuzz_crash_062.md:3:1:3:1
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



**UNDEFINED VARIABLE**
Nothing is named **s** in this scope.
Is there an **import** or **exposing** missing up-top?

**fuzz_crash_062.md:2:4:2:5:**
```roc
as s|||0
```
   ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_or
    (Expr.lookup "s")
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 11
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 -> #10)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 -> #7)
~~~
# TYPES
~~~roc
~~~
