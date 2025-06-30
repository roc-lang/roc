# META
~~~ini
description=Negative integer literal canonicalization
type=expr
~~~
# SOURCE
~~~roc
-123
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.5 (raw "-123"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.5 (value "-123"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Num(*)"))
~~~
