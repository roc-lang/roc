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
return Bool.True
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Stmt.ret)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# Type checking for this node type not yet implemented
~~~
