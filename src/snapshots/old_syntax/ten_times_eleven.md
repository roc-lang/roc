# META
~~~ini
description=ten_times_eleven
type=expr
~~~
# SOURCE
~~~roc
10*11
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:3),OpStar(1:3-1:4),Int(1:4-1:6),EndOfFile(1:6-1:6),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.6 (op "*")
	(e-int @1.1-1.3 (raw "10"))
	(e-int @1.4-1.6 (raw "11")))
~~~
# FORMATTED
~~~roc
10 * 11
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.6 (op "mul") (id 75)
	(e-int @1.1-1.3 (value "10"))
	(e-int @1.4-1.6 (value "11")))
~~~
# TYPES
~~~clojure
(expr (id 75) (type "*"))
~~~
