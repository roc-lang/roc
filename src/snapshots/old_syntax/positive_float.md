# META
~~~ini
description=positive_float
type=expr
~~~
# SOURCE
~~~roc
42.9
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(e-frac @1.1-1.5 (raw "42.9"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dec-small @1.1-1.5 (numerator "429") (denominator-power-of-ten "1") (value "42.9") (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Frac(*)"))
~~~
