# META
~~~ini
description=Binary operation expression simple addition
type=expr
~~~
# SOURCE
~~~roc
1 + 2
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpPlus(1:3-1:4),Int(1:5-1:6),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-binop @1.1-2.2 (op "+")
	(e-int @1.1-1.2 (raw "1"))
	(e-int @1.5-1.6 (raw "2")))
~~~
# FORMATTED
~~~roc
1 + 2
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-2.2 (op "add")
	(e-int @1.1-1.2 (value "1"))
	(e-int @1.5-1.6 (value "2")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.2 (type "*"))
~~~
