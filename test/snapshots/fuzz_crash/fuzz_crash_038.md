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
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.



**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
  (Stmt.import)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
