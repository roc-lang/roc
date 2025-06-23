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
NIL
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
		(int_var 16)
		(requirements (sign_needed "false") (bits_needed "types.types.Num.Int.BitsNeeded.7"))
		(value "42")))
~~~
# TYPES
~~~clojure
(expr 17 (type "*"))
~~~