# META
~~~ini
description=Zero as a decimal literal
type=expr
~~~
# SOURCE
~~~roc
0.0
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:4),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-frac @1.1-1.4 (raw "0.0"))
~~~
# FORMATTED
~~~roc
0.0
~~~
# CANONICALIZE
~~~clojure
(e-dec-small @1.1-1.4 (numerator "0") (denominator-power-of-ten "1") (value "0.0"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "Frac(*)"))
~~~
