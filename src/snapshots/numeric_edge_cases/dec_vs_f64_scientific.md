# META
~~~ini
description=Scientific notation uses Dec when value fits
type=expr
~~~
# SOURCE
~~~roc
1.5e2
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
(e-frac @1.1-1.6 (raw "1.5e2"))
~~~
# FORMATTED
~~~roc
1.5e2
~~~
# CANONICALIZE
~~~clojure
(e-dec-small @1.1-1.6 (numerator "150") (denominator-power-of-ten "0") (value "150"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.6 (type "Frac(*)"))
~~~
