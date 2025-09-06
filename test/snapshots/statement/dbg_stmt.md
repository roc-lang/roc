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
; Total type variables: 0
~~~
# TYPES
~~~roc
~~~
