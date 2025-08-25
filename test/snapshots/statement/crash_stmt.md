# META
~~~ini
description=Debug expression stmt
type=statement
~~~
# SOURCE
~~~roc
crash "some message"
~~~
# TOKENS
~~~text
KwCrash String ~~~
# PARSE
~~~clojure
(crash <statement>)
~~~
# FORMATTED
~~~roc
crash "some message"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
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
