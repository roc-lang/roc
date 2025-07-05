# META
~~~ini
description=Maximum value for i32 (2147483647)
type=expr
~~~
# SOURCE
~~~roc
2147483647
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:11),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.11 (raw "2147483647"))
~~~
# FORMATTED
~~~roc
2147483647
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.11 (value "2147483647"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.11 (type "Num(*)"))
~~~
