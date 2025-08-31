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
~~~
# FORMATTED
~~~roc
module []

!0
 | .t
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_073.md:1:11:1:12:**
```roc
module[]!0.t
```
          ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_073.md:1:9:1:10:**
```roc
module[]!0.t
```
        ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_073.md:1:11:1:14:**
```roc
module[]!0.t
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
