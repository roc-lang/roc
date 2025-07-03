# META
~~~ini
description=negative_int
type=expr
~~~
# SOURCE
~~~roc
-42
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:4),EndOfFile(1:4-1:4),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.4 (raw "-42"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.4 (value "-42"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "Num(a)"))
~~~
