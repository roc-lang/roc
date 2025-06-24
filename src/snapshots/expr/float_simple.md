# META
~~~ini
description=Simple float literal
type=expr
~~~
# SOURCE
~~~roc
3.14
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(frac (1:1-1:5) "3.14")
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_dec_small (1:1-1:5)
	(num_var 14)
	(requirements (fits_in_f32 "false") (fits_in_dec "true"))
	(before_decimal "3")
	(after_decimal "14")
	(after_decimal_digits "2"))
~~~
# TYPES
~~~clojure
(expr 14 (type "Num(FloatingPoint(*))"))
~~~