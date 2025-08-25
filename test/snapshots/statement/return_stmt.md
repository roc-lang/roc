# META
~~~ini
description=Debug expression stmt
type=statement
~~~
# SOURCE
~~~roc
return Bool.True
~~~
# TOKENS
~~~text
KwReturn UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(ret <statement>)
~~~
# FORMATTED
~~~roc
<ret>
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:8 to 1:12

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
