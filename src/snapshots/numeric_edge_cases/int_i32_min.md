# META
~~~ini
description=Minimum value for i32 (-2147483648)
type=expr
~~~
# SOURCE
~~~roc
-2147483648
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:12),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.12 (raw "-2147483648"))
~~~
# FORMATTED
~~~roc
-2147483648
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.12 (value "-2147483648"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.12 (type "Num(*)"))
~~~
