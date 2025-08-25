# META
~~~ini
description=Debug expression stmt
type=statement
~~~
# SOURCE
~~~roc
expect Bool.True
~~~
# TOKENS
~~~text
KwExpect UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(expect
  (binop_pipe
    (uc "Bool")
    (uc "True")
  )
)
~~~
# FORMATTED
~~~roc
expect Bool.True
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:1 to 1:16

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
# Type checking for this node type not yet implemented
~~~
