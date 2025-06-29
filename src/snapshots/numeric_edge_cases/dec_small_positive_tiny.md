# META
~~~ini
description=Tiny positive decimal that fits in dec_small
type=expr
~~~
# SOURCE
~~~roc
0.0001
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:7),EndOfFile(1:7-1:7),
~~~
# PARSE
~~~clojure
(e-frac @1-1-1-7 (raw "0.0001"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dec-small @1-1-1-7 (numerator "1") (denominator-power-of-ten "4") (value "0.0001") (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Frac(*)"))
~~~
