# META
~~~ini
description=newline_after_mul
type=expr
~~~
# SOURCE
~~~roc
3  *
  4
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpStar(1:4-1:5),Newline(1:1-1:1),
Int(2:3-2:4),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-binop @1.1-3.2 (op "*")
	(e-int @1.1-1.2 (raw "3"))
	(e-int @2.3-2.4 (raw "4")))
~~~
# FORMATTED
~~~roc
3 *
	4
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-3.2 (op "mul")
	(e-int @1.1-1.2 (value "3"))
	(e-int @2.3-2.4 (value "4")))
~~~
# TYPES
~~~clojure
(expr @1.1-3.2 (type "*"))
~~~
