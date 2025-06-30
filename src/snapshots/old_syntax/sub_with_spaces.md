# META
~~~ini
description=sub_with_spaces
type=expr
~~~
# SOURCE
~~~roc
1  -   2
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpBinaryMinus(1:4-1:5),Int(1:8-1:9),EndOfFile(1:9-1:9),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.9 (op "-")
	(e-int @1.1-1.2 (raw "1"))
	(e-int @1.8-1.9 (raw "2")))
~~~
# FORMATTED
~~~roc
1 - 2
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.9 (op "sub") (id 75)
	(e-int @1.1-1.2 (value "1"))
	(e-int @1.8-1.9 (value "2")))
~~~
# TYPES
~~~clojure
(expr (id 75) (type "*"))
~~~
