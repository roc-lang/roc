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
**Unsupported Node**
at 1:1 to 1:4

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
