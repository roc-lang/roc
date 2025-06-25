# META
~~~ini
description=Scientific notation float literal
type=expr
~~~
# SOURCE
~~~roc
1.23e-4
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:8),EndOfFile(1:8-1:8),
~~~
# PARSE
~~~clojure
(frac (1:1-1:8) "1.23e-4")
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_frac_f64 (1:1-1:8)
	(frac_var 14)
	(requirements (fits_in_f32 "true") (fits_in_dec "false"))
	(value "0.000123"))
~~~
# TYPES
~~~clojure
(expr 14 (type "Num(FloatingPoint(*))"))
~~~