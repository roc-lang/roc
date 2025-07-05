# META
~~~ini
description=float_with_underscores
type=expr
~~~
# SOURCE
~~~roc
-1_23_456.0_1_23_456
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:21),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-frac @1.1-1.21 (raw "-1_23_456.0_1_23_456"))
~~~
# FORMATTED
~~~roc
-1_23_456.0_1_23_456
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec @1.1-1.21 (value "-123456.01234560001"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.21 (type "Frac(*)"))
~~~
