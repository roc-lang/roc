# META
~~~ini
description=spaces_inside_empty_list
type=expr
~~~
# SOURCE
~~~roc
[  ]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),CloseSquare(1:4-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(e-list @1.1-1.5)
~~~
# FORMATTED
~~~roc
[]
~~~
# CANONICALIZE
~~~clojure
(e-empty_list @1.1-1.5)
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "List(*)"))
~~~
