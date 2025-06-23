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
(float (1:1-1:8) "1.23e-4")
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_float (1:1-1:8)
	(frac_var 73)
	(precision_var 72)
	(literal "1.23e-4")
	(value "0")
	(bound "f32"))
~~~
# TYPES
~~~clojure
(expr 74 (type "Num(FloatingPoint(*))"))
~~~