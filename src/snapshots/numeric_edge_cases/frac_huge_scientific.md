# META
~~~ini
description=Very large number in scientific notation
type=expr
~~~
# SOURCE
~~~roc
1.0e100
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
(e-frac @1.1-1.8 (raw "1.0e100"))
~~~
# FORMATTED
~~~roc
1.0e100
~~~
# CANONICALIZE
~~~clojure
(e-frac-f64 @1.1-1.8 (value "1e100"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.8 (type "Frac(*)"))
~~~
