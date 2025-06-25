# META
~~~ini
description=Binary operation expression simple addition
type=expr
~~~
# SOURCE
~~~roc
1 + 2
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpPlus(1:3-1:4),Int(1:5-1:6),EndOfFile(1:6-1:6),
~~~
# PARSE
~~~clojure
(e-binop @1-1-1-6 (op "+")
	(e-int @1-1-1-2 (raw "1"))
	(e-int @1-5-1-6 (raw "2")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-binop @1-1-1-6 (op "add") (id 78)
	(e-int @1-1-1-2 (num-var 74) (sign-needed "false") (bits-needed "7") (value "1"))
	(e-int @1-5-1-6 (num-var 77) (sign-needed "false") (bits-needed "7") (value "2")))
~~~
# TYPES
~~~clojure
(expr (id 78) (type "*"))
~~~