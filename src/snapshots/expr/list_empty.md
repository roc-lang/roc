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
(e-list @1-1-1-3)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1-1-1-3 (elem-var 72) (id 73)
	(elems))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "List(*)"))
~~~
