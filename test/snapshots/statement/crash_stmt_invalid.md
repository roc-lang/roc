# META
~~~ini
description=Crash statement with invalid non-string argument
type=statement
~~~
# SOURCE
~~~roc
crash 42
~~~
# TOKENS
~~~text
KwCrash Int ~~~
# PARSE
~~~clojure
(crash <statement>)
~~~
# FORMATTED
~~~roc
crash 42
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
