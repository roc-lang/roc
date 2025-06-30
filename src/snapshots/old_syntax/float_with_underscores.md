# META
~~~ini
description=float_with_underscores
type=expr
~~~
# SOURCE
~~~roc
-1_23_456.0_1_23_456
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:21),EndOfFile(1:21-1:21),
~~~
# PARSE
~~~clojure
(e-frac @1.1-1.21 (raw "-1_23_456.0_1_23_456"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec @1.1-1.21 (value "-123456.01234560001") (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Frac(*)"))
~~~
