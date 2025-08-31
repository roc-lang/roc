# META
~~~ini
description=Module dot malformed (should error)
type=expr
~~~
# SOURCE
~~~roc
I.5
~~~
# TOKENS
~~~text
UpperIdent Dot Int ~~~
# PARSE
~~~clojure
(binop_pipe
  (uc "I")
  (num_literal_i32 5)
)
~~~
# FORMATTED
~~~roc
I | 5
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**module_dot_tuple.md:1:1:1:4:**
```roc
I.5
```
^^^


# CANONICALIZE
~~~clojure
(Stmt.malformed)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# No expression found
~~~
