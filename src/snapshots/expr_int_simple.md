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
	(int_var 14)
	(requirements (sign_needed "false") (bits_needed "types.types.Num.Int.BitsNeeded.7"))
	(value "42"))
~~~
# TYPES
~~~clojure
(expr 14 (type "Num(Int(*))"))
~~~