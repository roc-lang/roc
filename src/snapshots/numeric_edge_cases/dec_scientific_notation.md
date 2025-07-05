# META
~~~ini
description=Dec literal with scientific notation
type=expr
~~~
# SOURCE
~~~roc
1.23456789012345678e10
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:23),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-frac @1.1-1.23 (raw "1.23456789012345678e10"))
~~~
# FORMATTED
~~~roc
1.23456789012345678e10
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec @1.1-1.23 (value "1.2345678901234568e10"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.23 (type "Frac(*)"))
~~~
