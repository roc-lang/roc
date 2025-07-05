# META
~~~ini
description=pizza_dbg
type=expr
~~~
# SOURCE
~~~roc
1 |> dbg
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpPizza(1:3-1:5),KwDbg(1:6-1:9),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.2 (raw "1"))
~~~
# FORMATTED
~~~roc
1
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.2 (value "1"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Num(*)"))
~~~
