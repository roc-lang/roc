# META
~~~ini
description=Minimum value for i8 (-128)
type=expr
~~~
# SOURCE
~~~roc
-128
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
(e-int @1.1-1.5 (raw "-128"))
~~~
# FORMATTED
~~~roc
-128
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.5 (value "-128"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Num(*)"))
~~~
