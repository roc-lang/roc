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
~~~txt
NIL
~~~
# TOKENS
~~~zig
Float(1:1-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(float (1:1-1:5) "-2.5")
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e_float (1:1-1:5)
	(frac_var 13)
	(precision_var 12)
	(literal "-2.5")
	(value "0")
	(bound "f32"))
~~~
# TYPES
~~~clojure
(expr 14 (type "Num(FloatingPoint(*))"))
~~~