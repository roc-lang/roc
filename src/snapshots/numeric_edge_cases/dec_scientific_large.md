# META
~~~ini
description=Large scientific notation that fits in Dec
type=expr
~~~
# SOURCE
~~~roc
1.5e18
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:7),EndOfFile(1:7-1:7),
~~~
# PARSE
~~~clojure
(e-frac @1-1-1-7 (raw "1.5e18"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec @1-1-1-7 (value "1.5e18") (id 72))
~~~
# TYPES
~~~clojure
(expr (id 72) (type "Frac(*)"))
~~~
