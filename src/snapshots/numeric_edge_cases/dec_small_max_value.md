# META
~~~ini
description=Maximum positive value that fits in dec_small (i16 max)
type=expr
~~~
# SOURCE
~~~roc
327.67
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:7),EndOfFile(1:7-1:7),
~~~
# PARSE
~~~clojure
(e-frac @1-1-1-7 (raw "327.67"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dec-small @1-1-1-7 (num-var 74) (fits-in-f32 "true") (fits-in-dec "true") (numerator "32767") (denominator-power-of-ten "2") (value "327.67") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Num(Fraction(*))"))
~~~