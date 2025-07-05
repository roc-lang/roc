# META
~~~ini
description=Scientific notation float literal
type=expr
~~~
# SOURCE
~~~roc
1.23e-4
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
(e-frac @1.1-1.8 (raw "1.23e-4"))
~~~
# FORMATTED
~~~roc
1.23e-4
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec @1.1-1.8 (value "0.000123"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.8 (type "Frac(*)"))
~~~
