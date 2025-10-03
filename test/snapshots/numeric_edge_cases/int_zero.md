# META
~~~ini
description=Integer zero
type=expr
~~~
# SOURCE
~~~roc
0
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.2 (raw "0"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-num @1.1-1.2 (value "0"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Num(_size)"))
~~~
