# META
~~~ini
description=Empty tuple literal
type=expr
~~~
# SOURCE
~~~roc
()
~~~
# EXPECTED
empty_tuple - tuple_empty_unbound.md:1:1:1:3
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),CloseRound(1:2-1:3),EndOfFile(1:3-1:3),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-1.3)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "empty_tuple"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.3 (type "Error"))
~~~
