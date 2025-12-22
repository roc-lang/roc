# META
~~~ini
description=Mono test: empty list
type=mono
~~~
# SOURCE
~~~roc
[]
~~~
# MONO
~~~roc
[]
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list)
~~~
# CANONICALIZE
~~~clojure
(e-empty_list)
~~~
# TYPES
~~~clojure
(expr (type "List(_a)"))
~~~
