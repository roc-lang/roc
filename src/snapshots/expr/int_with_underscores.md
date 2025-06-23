# META
~~~ini
description=Integer literal with underscores
type=expr
~~~
# SOURCE
~~~roc
1_000_000
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:10),EndOfFile(1:10-1:10),
~~~
# PARSE
~~~clojure
(int (1:1-1:10) "1_000_000")
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_int (1:1-1:10)
	(int_var 73)
	(precision_var 72)
	(literal "1_000_000")
	(value "TODO")
	(bound "u8"))
~~~
# TYPES
~~~clojure
(expr 74 (type "Num(Int(*))"))
~~~