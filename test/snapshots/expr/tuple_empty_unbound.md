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
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple)
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
(expr (type "Error"))
~~~
