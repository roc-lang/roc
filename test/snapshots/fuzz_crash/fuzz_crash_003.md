# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
= "te
~~~
# TOKENS
~~~text
OpAssign MalformedString ~~~
# PARSE
~~~clojure
(block
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc
"te
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token ** ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_003.md:1:2:1:3:**
```roc
= "te
```
 ^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **"te** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_003.md:1:3:1:6:**
```roc
= "te
```
  ^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_003.md:1:2:1:3:**
```roc
= "te
```
 ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_003.md:1:3:1:6:**
```roc
= "te
```
  ^^^


# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
