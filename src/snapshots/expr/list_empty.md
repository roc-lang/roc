# META
~~~ini
description=Empty list literal
type=expr
~~~
# SOURCE
~~~roc
[]
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),CloseSquare(1:2-1:3),EndOfFile(1:3-1:3),
~~~
# PARSE
~~~clojure
(e-list @1.1-1.3)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-empty_list @1.1-1.3)
~~~
# TYPES
~~~clojure
(expr @1.1-1.3 (type "List(*)"))
~~~
