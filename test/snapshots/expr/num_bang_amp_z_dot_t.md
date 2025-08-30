# META
~~~ini
description=num_bang_amp_z_dot_t
type=expr
~~~
# SOURCE
~~~roc
4
!
&z.t
~~~
# TOKENS
~~~text
Int OpBang OpAmpersand LowerIdent Dot LowerIdent ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
!
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:1 to 3:1

**Unsupported Node**
at 2:1 to 3:1

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
