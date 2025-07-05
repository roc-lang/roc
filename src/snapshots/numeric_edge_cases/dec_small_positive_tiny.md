# META
~~~ini
description=Tiny positive decimal that fits in dec_small
type=expr
~~~
# SOURCE
~~~roc
0.0001
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
(e-frac @1.1-1.7 (raw "0.0001"))
~~~
# FORMATTED
~~~roc
0.0001
~~~
# CANONICALIZE
~~~clojure
(e-dec-small @1.1-1.7 (numerator "1") (denominator-power-of-ten "4") (value "0.0001"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.7 (type "Frac(*)"))
~~~
