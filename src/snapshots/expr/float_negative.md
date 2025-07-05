# META
~~~ini
description=Negative float literal
type=expr
~~~
# SOURCE
~~~roc
-2.5
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:5),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-frac @1.1-1.5 (raw "-2.5"))
~~~
# FORMATTED
~~~roc
-2.5
~~~
# CANONICALIZE
~~~clojure
(e-dec-small @1.1-1.5 (numerator "-25") (denominator-power-of-ten "1") (value "-2.5"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Frac(*)"))
~~~
