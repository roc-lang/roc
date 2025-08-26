# META
~~~ini
description=Maximum value for i32 (2147483647)
type=expr
~~~
# SOURCE
~~~roc
2147483647
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:11),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.11 (raw "2147483647"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.11 (value "2147483647"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.11 (type "Num(_size)"))
~~~
