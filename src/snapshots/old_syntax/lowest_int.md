# META
~~~ini
description=lowest_int
type=expr
~~~
# SOURCE
~~~roc
-9223372036854775808
~~~
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
(e-int @1.1-1.21 (value "-9223372036854775808") (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Num(*)"))
~~~
