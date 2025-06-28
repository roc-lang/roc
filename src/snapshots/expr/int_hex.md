# META
~~~ini
description=Hexadecimal integer literal
type=expr
~~~
# SOURCE
~~~roc
0xFF
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(e-int @1-1-1-5 (raw "0xFF"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1-1-1-5 (value "255") (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Int(*)"))
~~~
