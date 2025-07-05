# META
~~~ini
description=negative_float
type=expr
~~~
# SOURCE
~~~roc
-42.9
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:6),EndOfFile(1:6-1:6),
~~~
# PARSE
~~~clojure
(e-frac @1.1-1.6 (raw "-42.9"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dec-small @1.1-1.6 (numerator "-429") (denominator-power-of-ten "1") (value "-42.9"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.6 (type "Frac(*)"))
~~~
