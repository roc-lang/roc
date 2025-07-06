# META
~~~ini
description=one_plus_two
type=expr
~~~
# SOURCE
~~~roc
1+2
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpPlus(1:2-1:3),Int(1:3-1:4),EndOfFile(1:4-1:4),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.4 (op "+")
	(e-int @1.1-1.2 (raw "1"))
	(e-int @1.3-1.4 (raw "2")))
~~~
# FORMATTED
~~~roc
1 + 2
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.4 (op "add")
	(e-int @1.1-1.2 (value "1"))
	(e-int @1.3-1.4 (value "2")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "*"))
~~~
