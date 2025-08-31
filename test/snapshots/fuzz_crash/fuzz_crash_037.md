# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]"\
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare MalformedString ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

"\
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **"\** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_037.md:1:9:1:11:**
```roc
module[]"\
```
        ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_037.md:1:9:1:11:**
```roc
module[]"\
```
        ^^


# CANONICALIZE
~~~clojure
(Expr.block
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
