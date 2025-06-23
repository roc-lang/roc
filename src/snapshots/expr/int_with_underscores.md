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
	(int_var 14)
	(requirements (sign_needed "false") (bits_needed "types.types.Num.Int.BitsNeeded.17_to_31"))
	(value "1000000"))
~~~
# TYPES
~~~clojure
(expr 14 (type "Num(Int(*))"))
~~~