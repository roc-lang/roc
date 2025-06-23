# META
~~~ini
description=Large integer literal
type=expr
~~~
# SOURCE
~~~roc
999999999999999999999999999999
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:31),EndOfFile(1:31-1:31),
~~~
# PARSE
~~~clojure
(int (1:1-1:31) "999999999999999999999999999999")
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_int (1:1-1:31)
	(int_var 13)
	(requirements
		(sign_needed "false")
		(bits_needed "100"))
	(value "999999999999999999999999999999"))
~~~
# TYPES
~~~clojure
(expr 14 (type "Num(Int(*))"))
~~~
