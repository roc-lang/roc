# META
~~~ini
description=Maximum value for i8 (127)
type=expr
~~~
# SOURCE
~~~roc
127
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:4),EndOfFile(1:4-1:4),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.4 (raw "127"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.4 (value "127"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "Num(*)"))
~~~
