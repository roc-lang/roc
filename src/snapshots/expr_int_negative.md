# META
~~~ini
description=Negative integer literal canonicalization
type=expr
~~~
# SOURCE
~~~roc
-123
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(int (1:1-1:5) "-123")
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_int (1:1-1:5)
	(int_var 73)
	(precision_var 72)
	(literal "-123")
	(value "TODO")
	(bound "u8"))
~~~
# TYPES
~~~clojure
(expr 74 (type "Num(Int(*))"))
~~~