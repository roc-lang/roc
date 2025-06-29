# META
~~~ini
description=Minimum value for i32 (-2147483648)
type=expr
~~~
# SOURCE
~~~roc
-2147483648
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:12),EndOfFile(1:12-1:12),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.12 (raw "-2147483648"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.12 (value "-2147483648") (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Num(*)"))
~~~
