# META
~~~ini
description=Chained record field (dot-access)
type=expr
~~~
# SOURCE
~~~roc
person.address.street
~~~
# TOKENS
~~~text
LowerIdent Dot LowerIdent Dot LowerIdent ~~~
# PARSE
~~~clojure
(binop_pipe
  (binop_pipe
    (lc "person")
    (dot_lc "address")
  )
  (dot_lc "street")
)
~~~
# FORMATTED
~~~roc
person.address | .street
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**record_chained_access.md:1:1:1:22:**
```roc
person.address.street
```
^^^^^^^^^^^^^^^^^^^^^


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
