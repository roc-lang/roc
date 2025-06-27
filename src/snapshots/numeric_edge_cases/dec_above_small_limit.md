# META
~~~ini
description=Decimal just above dec_small limit that requires frac_dec
type=expr
~~~
# SOURCE
~~~roc
32768.0
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:8),EndOfFile(1:8-1:8),
~~~
# PARSE
~~~clojure
(e-frac @1-1-1-8 (raw "32768.0"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec @1-1-1-8 (value "32768") (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Frac(*)"))
~~~