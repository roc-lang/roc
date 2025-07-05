# META
~~~ini
description=newline_before_add
type=expr
~~~
# SOURCE
~~~roc
3
+ 4
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),Newline(1:1-1:1),
OpPlus(2:1-2:2),Int(2:3-2:4),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-binop @1.1-3.2 (op "+")
	(e-int @1.1-1.2 (raw "3"))
	(e-int @2.3-2.4 (raw "4")))
~~~
# FORMATTED
~~~roc
3
	+ 4
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-3.2 (op "add")
	(e-int @1.1-1.2 (value "3"))
	(e-int @2.3-2.4 (value "4")))
~~~
# TYPES
~~~clojure
(expr @1.1-3.2 (type "*"))
~~~
