# META
~~~ini
description=Same number as dec_vs_f64_scientific but in decimal form
type=expr
~~~
# SOURCE
~~~roc
150.0
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Float,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-frac (raw "150.0"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dec-small (numerator "1500") (denominator-power-of-ten "1") (value "150"))
~~~
# TYPES
~~~clojure
(expr (type "Num(Frac(_size))"))
~~~
