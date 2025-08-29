# META
~~~ini
description=Debug expression stmt
type=statement
~~~
# SOURCE
~~~roc
dbg Bool.True
~~~
# TOKENS
~~~text
KwDbg UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
 
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:4 to 1:5

**Unsupported Node**
at 1:4 to 1:5

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
