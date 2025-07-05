# META
~~~ini
description=Maximum value for i64 (9223372036854775807)
type=expr
~~~
# SOURCE
~~~roc
9223372036854775807
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:20),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.20 (raw "9223372036854775807"))
~~~
# FORMATTED
~~~roc
9223372036854775807
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.20 (value "9223372036854775807"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.20 (type "Num(*)"))
~~~
