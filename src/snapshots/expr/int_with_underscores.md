# META
~~~ini
description=Integer literal with underscores
type=expr
~~~
# SOURCE
~~~roc
1_000_000
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:10),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.10 (raw "1_000_000"))
~~~
# FORMATTED
~~~roc
1_000_000
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.10 (value "1000000"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.10 (type "Num(*)"))
~~~
