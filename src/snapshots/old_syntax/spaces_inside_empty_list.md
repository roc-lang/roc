# META
~~~ini
description=spaces_inside_empty_list
type=expr
~~~
# SOURCE
~~~roc
[  ]
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),CloseSquare(1:4-1:5),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
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
