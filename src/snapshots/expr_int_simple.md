# META
~~~ini
description=Simple integer literal canonicalization
type=expr
~~~
# SOURCE
~~~roc
42
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:3),EndOfFile(1:3-1:3),
~~~
# PARSE
~~~clojure
(int (1:1-1:3) "42")
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_int (1:1-1:3)
	(int_var 73)
	(precision_var 72)
	(literal "42")
	(value "TODO")
	(bound "u8"))
~~~
# TYPES
~~~clojure
(expr 74 (type "Num(Int(*))"))
~~~