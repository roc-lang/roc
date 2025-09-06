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
(empty)
~~~
# FORMATTED
~~~roc

~~~
# EXPECTED
CRASH EXPECTS STRING - crash_stmt_invalid.md:1:1:1:9
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
