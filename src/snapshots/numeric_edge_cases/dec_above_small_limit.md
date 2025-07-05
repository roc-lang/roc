# META
~~~ini
description=Decimal just above dec_small limit that requires frac_dec
type=expr
~~~
# SOURCE
~~~roc
32768.0
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:8),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-frac @1.1-1.8 (raw "32768.0"))
~~~
# FORMATTED
~~~roc
32768.0
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec @1.1-1.8 (value "32768"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.8 (type "Frac(*)"))
~~~
