# META
~~~ini
description=newline_inside_empty_list
type=expr
~~~
# SOURCE
~~~roc
[
]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),Newline(1:1-1:1),
CloseSquare(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-list @1.1-2.2)
~~~
# FORMATTED
~~~roc
[]
~~~
# CANONICALIZE
~~~clojure
(e-empty_list @1.1-2.2)
~~~
# TYPES
~~~clojure
(expr @1.1-2.2 (type "List(*)"))
~~~
