# META
~~~ini
description=Maximum value for i32 (2147483647)
type=expr
~~~
# SOURCE
~~~roc
2147483647
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:11),EndOfFile(1:11-1:11),
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
(expr @1.1-1.11 (type "Num(*)"))
~~~
