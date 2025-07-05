# META
~~~ini
description=Same number as dec_vs_f64_scientific but in decimal form
type=expr
~~~
# SOURCE
~~~roc
150.0
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:6),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-frac @1.1-1.6 (raw "150.0"))
~~~
# FORMATTED
~~~roc
150.0
~~~
# CANONICALIZE
~~~clojure
(e-dec-small @1.1-1.6 (numerator "1500") (denominator-power-of-ten "1") (value "150"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.6 (type "Frac(*)"))
~~~
