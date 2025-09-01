# META
~~~ini
description=Field access expression simple expression
type=expr
~~~
# SOURCE
~~~roc
person.name
~~~
# TOKENS
~~~text
LowerIdent Dot LowerIdent ~~~
# PARSE
~~~clojure
(binop_pipe
  (lc "person")
  (dot_lc "name")
)
~~~
# FORMATTED
~~~roc
person.name
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**field_access.md:1:1:1:12:**
```roc
person.name
```
^^^^^^^^^^^


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
