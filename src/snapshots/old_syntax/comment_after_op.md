# META
~~~ini
description=comment_after_op
type=expr
~~~
# SOURCE
~~~roc
12  * # test!
 92
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:3),OpStar(1:5-1:6),Newline(1:8-1:14),
Int(2:2-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-binop @1.1-2.4 (op "*")
	(e-int @1.1-1.3 (raw "12"))
	(e-int @2.2-2.4 (raw "92")))
~~~
# FORMATTED
~~~roc
12 * # test!
	92
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-2.4 (op "mul")
	(e-int @1.1-1.3 (value "12"))
	(e-int @2.2-2.4 (value "92")))
~~~
# TYPES
~~~clojure
(expr @1.1-2.4 (type "*"))
~~~
