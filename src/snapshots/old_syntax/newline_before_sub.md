# META
~~~ini
description=newline_before_sub
type=expr
~~~
# SOURCE
~~~roc
3
- 4
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),Newline(1:1-1:1),
OpBinaryMinus(2:1-2:2),Int(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-binop @1.1-2.4 (op "-")
	(e-int @1.1-1.2 (raw "3"))
	(e-int @2.3-2.4 (raw "4")))
~~~
# FORMATTED
~~~roc
3
	- 4
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-2.4 (op "sub") (id 75)
	(e-int @1.1-1.2 (value "3"))
	(e-int @2.3-2.4 (value "4")))
~~~
# TYPES
~~~clojure
(expr (id 75) (type "*"))
~~~
