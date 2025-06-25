# META
~~~ini
description=Zero as a decimal literal
type=expr
~~~
# SOURCE
~~~roc
0.0
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:4),EndOfFile(1:4-1:4),
~~~
# PARSE
~~~clojure
(e-frac @1-1-1-4 (raw "0.0"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dec-small @1-1-1-4 (num-var 74) (fits-in-f32 "true") (fits-in-dec "true") (numerator "0") (denominator-power-of-ten "1") (value "0e0") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Num(FloatingPoint(*))"))
~~~