# META
~~~ini
description=empty_list
type=expr
~~~
# SOURCE
~~~roc
[]
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),CloseSquare(1:2-1:3),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-list @1.1-1.3)
~~~
# FORMATTED
~~~roc
[]
~~~
# CANONICALIZE
~~~clojure
(e-empty_list @1.1-1.3)
~~~
# TYPES
~~~clojure
(expr @1.1-1.3 (type "List(*)"))
~~~
