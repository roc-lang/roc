# META
~~~ini
description=Negative zero as a decimal literal
type=expr
~~~
# SOURCE
~~~roc
-0.0
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(e-frac @1-1-1-5 (raw "-0.0"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dec-small @1-1-1-5 (numerator "0") (denominator-power-of-ten "0") (value "0.0") (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Frac(*)"))
~~~
