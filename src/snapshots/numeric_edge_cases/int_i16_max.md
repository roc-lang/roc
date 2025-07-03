# META
~~~ini
description=Maximum value for i16 (32767)
type=expr
~~~
# SOURCE
~~~roc
32767
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:6),EndOfFile(1:6-1:6),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.6 (raw "32767"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.6 (value "32767"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.6 (type "Num(a)"))
~~~
