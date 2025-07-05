# META
~~~ini
description=Decimal with 8 decimal places
type=expr
~~~
# SOURCE
~~~roc
3.14159265
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:11),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-frac @1.1-1.11 (raw "3.14159265"))
~~~
# FORMATTED
~~~roc
3.14159265
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec @1.1-1.11 (value "3.14159265"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.11 (type "Frac(*)"))
~~~
