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
(list (1:1-1:3))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_list (1:1-1:3) (elem_var 72) (elems))
~~~
# TYPES
~~~clojure
(expr 73 (type "List(*)"))
~~~