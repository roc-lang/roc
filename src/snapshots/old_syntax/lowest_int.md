# META
~~~ini
description=lowest_int
type=expr
~~~
# SOURCE
~~~roc
-9223372036854775808
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:21),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.21 (raw "-9223372036854775808"))
~~~
# FORMATTED
~~~roc
-9223372036854775808
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.21 (value "-9223372036854775808"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.21 (type "Num(*)"))
~~~
