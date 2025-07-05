# META
~~~ini
description=add_with_spaces
type=expr
~~~
# SOURCE
~~~roc
1  +   2
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpPlus(1:4-1:5),Int(1:8-1:9),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-binop @1.1-2.2 (op "+")
	(e-int @1.1-1.2 (raw "1"))
	(e-int @1.8-1.9 (raw "2")))
~~~
# FORMATTED
~~~roc
1 + 2
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-2.2 (op "add")
	(e-int @1.1-1.2 (value "1"))
	(e-int @1.8-1.9 (value "2")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.2 (type "*"))
~~~
