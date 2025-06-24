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
	(e-int @1-1-1-2 (int-var 73) (precision-var 72) (literal "1") (value "TODO") (bound "u8"))
	(e-int @1-5-1-6 (int-var 76) (precision-var 75) (literal "2") (value "TODO") (bound "u8")))
~~~
# TYPES
~~~clojure
(expr (id 78) (type "*"))
~~~