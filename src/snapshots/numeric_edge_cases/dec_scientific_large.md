# META
~~~ini
description=Large scientific notation that fits in Dec
type=expr
~~~
# SOURCE
~~~roc
1.5e18
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
(e-frac @1.1-1.7 (raw "1.5e18"))
~~~
# FORMATTED
~~~roc
1.5e18
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec @1.1-1.7 (value "1.5e18"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.7 (type "Frac(*)"))
~~~
