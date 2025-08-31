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
NIL
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



**EXPRESSION IN STATEMENT CONTEXT**
Found an expression where a statement was expected.
This might be a missing semicolon or an incorrectly placed expression.

**fuzz_crash_001.md:1:1:1:3:**
```roc
mo|%
```
^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.



# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.malformed)
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
