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
(float (1:1-1:5) "3.14")
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_float (1:1-1:5)
	(frac_var 73)
	(precision_var 72)
	(literal "3.14")
	(value "0")
	(bound "f32"))
~~~
# TYPES
~~~clojure
(expr 74 (type "Num(FloatingPoint(*))"))
~~~