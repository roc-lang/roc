# META
~~~ini
description=Large integer literal
type=expr
~~~
# SOURCE
~~~roc
999999999999999999999999999999
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:31),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.31 (raw "999999999999999999999999999999"))
~~~
# FORMATTED
~~~roc
999999999999999999999999999999
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.31 (value "999999999999999999999999999999"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.31 (type "Num(*)"))
~~~
