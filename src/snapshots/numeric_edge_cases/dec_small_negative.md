# META
~~~ini
description=Small negative decimal that fits in dec_small
type=expr
~~~
# SOURCE
~~~roc
-3.14159
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:9),EndOfFile(1:9-1:9),
~~~
# PARSE
~~~clojure
(e-frac @1.1-1.9 (raw "-3.14159"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec @1.1-1.9 (value "-3.14159"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.9 (type "Frac(a)"))
~~~
