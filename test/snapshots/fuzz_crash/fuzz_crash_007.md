# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
ff8.8.d
~~~
# TOKENS
~~~text
LowerIdent Dot Int Dot LowerIdent ~~~
# PARSE
~~~clojure
(block
  (binop_pipe
    (binop_pipe
      (lc "ff8")
      (num_literal_i32 8)
    )
    (dot_lc "d")
  )
)
~~~
# FORMATTED
~~~roc
ff8 | 8.d | .d
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_crash_007.md:1:1:1:8:**
```roc
ff8.8.d
```
^^^^^^^


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
