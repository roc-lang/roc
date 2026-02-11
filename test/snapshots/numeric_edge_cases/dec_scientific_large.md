# META
~~~ini
description=Large scientific notation that fits in Dec
type=expr
~~~
# SOURCE
~~~roc
1.5e18
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Float,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-frac (raw "1.5e18"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec (value "1500000000000000000"))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
