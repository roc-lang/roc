# META
~~~ini
description=Small negative decimal that fits in dec_small
type=expr
~~~
# SOURCE
~~~roc
-3.14159
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:9),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-frac @1.1-1.9 (raw "-3.14159"))
~~~
# FORMATTED
~~~roc
-3.14159
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec @1.1-1.9 (value "-3.14159"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.9 (type "Frac(*)"))
~~~
