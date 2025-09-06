# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]!0.t
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare OpBang Int MalformedUnknownToken Dot LowerIdent ~~~
# PARSE
~~~clojure
(module-header)
(block
  (unary_not <unary_op>)
  (binop_pipe
    (malformed)
    (dot_lc "t")
  )
)
~~~
# FORMATTED
~~~roc
module []

!0
 | .t
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
PARSE ERROR - fuzz_crash_073.md:1:9:1:10
PARSE ERROR - fuzz_crash_073.md:1:10:1:11
PARSE ERROR - fuzz_crash_073.md:1:12:1:14
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_073.md:1:11:1:12:**
```roc
module[]!0.t
```
          ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.unary_not)
  (Expr.binop_pipe)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 7
(var #0 _)
(var #1 Num *)
(var #2 -> #1)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
~~~
# TYPES
~~~roc
~~~
