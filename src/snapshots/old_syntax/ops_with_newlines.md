# META
~~~ini
description=ops_with_newlines
type=expr
~~~
# SOURCE
~~~roc
3
+

  4
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),Newline(1:1-1:1),
OpPlus(2:1-2:2),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(4:3-4:4),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
~~~
# PARSE
~~~clojure
(e-binop @1.1-5.2 (op "+")
	(e-int @1.1-1.2 (raw "3"))
	(e-int @4.3-4.4 (raw "4")))
~~~
# FORMATTED
~~~roc
3
	+

	4
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-5.2 (op "add")
	(e-int @1.1-1.2 (value "3"))
	(e-int @4.3-4.4 (value "4")))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "*"))
~~~
