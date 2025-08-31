# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]()0     .t
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare OpenRound CloseRound Int Dot LowerIdent ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []()
0 | .t
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_070.md:1:9:1:11:**
```roc
module[]()0     .t
```
        ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_070.md:1:11:1:19:**
```roc
module[]()0     .t
```
          ^^^^^^^^


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
