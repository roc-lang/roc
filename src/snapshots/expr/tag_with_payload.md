# META
~~~ini
description=Tag with payload
type=expr
~~~
# SOURCE
~~~roc
Some(42)
~~~
# PROBLEMS
~~~txt
NIL
~~~
# TOKENS
~~~zig
UpperIdent(1:1-1:5),NoSpaceOpenRound(1:5-1:6),Int(1:6-1:8),CloseRound(1:8-1:9),EndOfFile(1:9-1:9),
~~~
# PARSE
~~~clojure
(apply (1:1-1:9)
	(tag (1:1-1:5) "Some")
	(int (1:6-1:8) "42"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_call (1:1-1:9)
	(e_tag (1:1-1:5)
		(ext_var 0)
		(name "Some")
		(args "TODO"))
	(e_int (1:6-1:8)
		(int_var 15)
		(precision_var 14)
		(literal "42")
		(value "TODO")
		(bound "u8")))
~~~
# TYPES
~~~clojure
(expr 17 (type "*"))
~~~