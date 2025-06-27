# META
~~~ini
description=Decimal that is exactly 1/10^n
type=expr
~~~
# SOURCE
~~~roc
0.001
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:6),EndOfFile(1:6-1:6),
~~~
# PARSE
~~~clojure
(e-frac @1-1-1-6 (raw "0.001"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dec-small @1-1-1-6 (num-var 74) (numerator "1") (denominator-power-of-ten "3") (value "0.001") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Frac(*)"))
~~~