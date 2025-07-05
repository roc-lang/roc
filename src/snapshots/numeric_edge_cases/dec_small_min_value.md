# META
~~~ini
description=Minimum negative value that fits in dec_small (i16 min)
type=expr
~~~
# SOURCE
~~~roc
-327.68
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
(e-frac @1.1-1.8 (raw "-327.68"))
~~~
# FORMATTED
~~~roc
-327.68
~~~
# CANONICALIZE
~~~clojure
(e-dec-small @1.1-1.8 (numerator "-32768") (denominator-power-of-ten "2") (value "-327.68"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.8 (type "Frac(*)"))
~~~
