# META
~~~ini
description=Scientific notation uses Dec when value fits
type=expr
~~~
# SOURCE
~~~roc
1.5e2
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:6),EndOfFile(1:6-1:6),
~~~
# PARSE
~~~clojure
(e-frac @1-1-1-6 (raw "1.5e2"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dec-small @1-1-1-6 (numerator "150") (denominator-power-of-ten "0") (value "150") (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Frac(*)"))
~~~
