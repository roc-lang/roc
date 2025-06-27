# META
~~~ini
description=Same number as dec_vs_f64_scientific but in decimal form
type=expr
~~~
# SOURCE
~~~roc
150.0
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:6),EndOfFile(1:6-1:6),
~~~
# PARSE
~~~clojure
(e-frac @1-1-1-6 (raw "150.0"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dec-small @1-1-1-6 (num-var 74) (numerator "1500") (denominator-power-of-ten "1") (value "150") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Frac(*)"))
~~~