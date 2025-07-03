# META
~~~ini
description=Maximum value for u8 (255)
type=expr
~~~
# SOURCE
~~~roc
255
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:4),EndOfFile(1:4-1:4),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.4 (raw "255"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.4 (value "255"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "Num(a)"))
~~~
