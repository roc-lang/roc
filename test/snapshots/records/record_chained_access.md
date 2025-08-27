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
**Unsupported Node**
at 1:1 to 1:22

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
