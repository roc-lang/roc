# META
~~~ini
description=Hexadecimal integer literal
type=expr
~~~
# SOURCE
~~~roc
0xFF
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(int (1:1-1:5) "0xFF")
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_int (1:1-1:5)
	(int_var 14)
	(requirements (sign_needed "false") (bits_needed "types.types.Num.Int.BitsNeeded.8"))
	(value "255"))
~~~
# TYPES
~~~clojure
(expr 14 (type "Num(Int(*))"))
~~~