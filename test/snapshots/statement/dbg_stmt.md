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
(empty)
~~~
# FORMATTED
~~~roc

~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(empty)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# Type checking for this node type not yet implemented
~~~
