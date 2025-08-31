# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0 (
~~~
# TOKENS
~~~text
Int OpenRound ~~~
# PARSE
~~~clojure
(block
  (apply_anon
    (num_literal_i32 0)
  )
)
~~~
# FORMATTED
~~~roc
0()
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_expr_apply_close_round**
This is an unexpected parsing error. Please check your syntax.

**fuzz_hang_001.md:1:1:1:4:**
```roc
0 (
```
^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**fuzz_hang_001.md:1:1:1:4:**
```roc
0 (
```
^^^


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
