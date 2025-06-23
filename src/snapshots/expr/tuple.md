# META
~~~ini
description=Tuple expression
type=expr
~~~
# SOURCE
~~~roc
(1, "hello", True)
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),Int(1:2-1:3),Comma(1:3-1:4),StringStart(1:5-1:6),StringPart(1:6-1:11),StringEnd(1:11-1:12),Comma(1:12-1:13),UpperIdent(1:14-1:18),CloseRound(1:18-1:19),EndOfFile(1:19-1:19),
~~~
# PARSE
~~~clojure
(tuple (1:1-1:19)
	(int (1:2-1:3) "1")
	(string (1:5-1:12) (string_part (1:6-1:11) "hello"))
	(tag (1:14-1:18) "True"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_tuple (1:1-1:19)
	(tuple_var "#19")
	(elems
		(e_int (1:2-1:3)
			(int_var 14)
			(requirements (sign_needed "false") (bits_needed "types.types.Num.Int.BitsNeeded.7"))
			(value "1"))
		(e_string (1:5-1:12) (e_literal (1:6-1:11) "hello"))
		(e_tag (1:14-1:18)
			(ext_var 0)
			(name "True")
			(args "TODO"))))
~~~
# TYPES
~~~clojure
(expr 20 (type "*"))
~~~