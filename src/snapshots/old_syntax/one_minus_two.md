# META
~~~ini
description=one_minus_two
type=expr
~~~
# SOURCE
~~~roc
1-2
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpBinaryMinus(1:2-1:3),Int(1:3-1:4),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-binop @1.1-2.2 (op "-")
	(e-int @1.1-1.2 (raw "1"))
	(e-int @1.3-1.4 (raw "2")))
~~~
# FORMATTED
~~~roc
1 - 2
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-2.2 (op "sub")
	(e-int @1.1-1.2 (value "1"))
	(e-int @1.3-1.4 (value "2")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.2 (type "*"))
~~~
