# META
~~~ini
description=Maximum positive value that fits in dec_small (i16 max)
type=expr
~~~
# SOURCE
~~~roc
327.67
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:7),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-frac @1.1-1.7 (raw "327.67"))
~~~
# FORMATTED
~~~roc
327.67
~~~
# CANONICALIZE
~~~clojure
(e-dec-small @1.1-1.7 (numerator "32767") (denominator-power-of-ten "2") (value "327.67"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.7 (type "Frac(*)"))
~~~
