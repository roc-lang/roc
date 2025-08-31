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
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_008.md:1:2:1:3:**
```roc
||1
```
 ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_008.md:1:1:1:5:**
```roc
||1
```
^^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
