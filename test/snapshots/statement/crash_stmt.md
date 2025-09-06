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
