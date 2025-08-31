# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
F
~~~
# TOKENS
~~~text
UpperIdent ~~~
# PARSE
~~~clojure
(block
  (uc "F")
)
~~~
# FORMATTED
~~~roc
F
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_004.md:1:1:1:2:**
```roc
F
```
^


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
