# META
~~~ini
description=Maximum value for u8 (255)
type=expr
~~~
# SOURCE
~~~roc
255
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:4),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.4 (raw "255"))
~~~
# FORMATTED
~~~roc
255
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.4 (value "255"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "Num(*)"))
~~~
