# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
||1
~~~
# TOKENS
~~~text
OpBar MalformedUnknownToken OpBar Int ~~~
# PARSE
~~~clojure
(block
  (lambda
    (body
      (num_literal_i32 1)
    )
    (args
      (malformed)
    )
  )
)
~~~
# FORMATTED
~~~roc
|| 1
~~~
# EXPECTED
ASCII CONTROL CHARACTER - :0:0:0:0
MISSING HEADER - fuzz_crash_008.md:1:1:1:2
PARSE ERROR - fuzz_crash_008.md:1:3:1:4
PARSE ERROR - fuzz_crash_008.md:1:4:1:5
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_008.md:1:2:1:3:**
```roc
||1
```
 ^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.lambda (canonicalized))
)
~~~
# SOLVED
~~~clojure
; Total type variables: 7
(var #0 _)
(var #1 _)
(var #2 Num *)
(var #3 -> #6)
(var #4 _)
(var #5 _)
(var #6 fn_pure)
~~~
# TYPES
~~~roc
~~~
