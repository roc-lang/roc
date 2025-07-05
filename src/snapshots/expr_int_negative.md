# META
~~~ini
description=Negative integer literal canonicalization
type=expr
~~~
# SOURCE
~~~roc
-123
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:5),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.5 (raw "-123"))
~~~
# FORMATTED
~~~roc
-123
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.5 (value "-123"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Num(*)"))
~~~
