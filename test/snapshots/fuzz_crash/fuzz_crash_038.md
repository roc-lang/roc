# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
*import B as
~~~
# TOKENS
~~~text
OpStar KwImport UpperIdent KwAs ~~~
# PARSE
~~~clojure
(block
  (malformed)
  (import
    (uc "B")
  )
)
~~~
# FORMATTED
~~~roc
import B
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_038.md:1:1:1:2
PARSE ERROR - fuzz_crash_038.md:1:2:1:8
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.



# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Stmt.import)
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
