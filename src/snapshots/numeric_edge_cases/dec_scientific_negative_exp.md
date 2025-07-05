# META
~~~ini
description=Dec literal with negative exponent scientific notation
type=expr
~~~
# SOURCE
~~~roc
1.23456789012345678e-10
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:24),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-frac @1.1-1.24 (raw "1.23456789012345678e-10"))
~~~
# FORMATTED
~~~roc
1.23456789012345678e-10
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec @1.1-1.24 (value "0.000000000123456789"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.24 (type "Frac(*)"))
~~~
