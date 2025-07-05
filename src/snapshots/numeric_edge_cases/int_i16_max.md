# META
~~~ini
description=Maximum value for i16 (32767)
type=expr
~~~
# SOURCE
~~~roc
32767
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:6),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.6 (raw "32767"))
~~~
# FORMATTED
~~~roc
32767
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.6 (value "32767"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.6 (type "Num(*)"))
~~~
