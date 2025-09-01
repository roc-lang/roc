# META
~~~ini
description=Invalid float literal too many decimal points
type=expr
~~~
# SOURCE
~~~roc
3.14.15
~~~
# TOKENS
~~~text
Float Dot Int ~~~
# PARSE
~~~clojure
(binop_pipe
  (frac_literal_small 3.14)
  (num_literal_i32 15)
)
~~~
# FORMATTED
~~~roc
3.14.15 | 15
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**float_invalid.md:1:1:1:8:**
```roc
3.14.15
```
^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
(expr :tag malformed :type "Error")
~~~
# TYPES
~~~roc
Error
~~~
