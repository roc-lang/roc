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
NO CHANGE
~~~
# EXPECTED
UNDEFINED VARIABLE - field_access.md:1:1:1:7
# PROBLEMS
**Unsupported Node**
at 1:1 to 1:11

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
