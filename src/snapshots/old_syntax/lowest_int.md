# META
~~~ini
description=lowest_int
type=expr
~~~
# SOURCE
~~~roc
-9223372036854775808
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:21),EndOfFile(1:21-1:21),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.21 (raw "-9223372036854775808"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.21 (value "-9223372036854775808"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.21 (type "Num(*)"))
~~~
