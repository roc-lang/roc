# META
~~~ini
description=Test empty list literal
type=expr
~~~
# SOURCE
~~~roc
[]
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
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-empty_list)
~~~
# TYPES
~~~clojure
(expr (type "List(_a)"))
~~~
