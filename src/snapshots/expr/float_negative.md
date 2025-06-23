# META
~~~ini
description=Negative float literal
type=expr
~~~
# SOURCE
~~~roc
-2.5
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(frac (1:1-1:5) "-2.5")
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_frac_dec (1:1-1:5)
	(frac_var 14)
	(requirements (fits_in_f32 "true") (fits_in_dec "true"))
	(value "-2.5"))
~~~
# TYPES
~~~clojure
(expr 14 (type "Num(FloatingPoint(*))"))
~~~