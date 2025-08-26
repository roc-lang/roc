# META
~~~ini
description=Very large number in scientific notation
type=expr
~~~
# SOURCE
~~~roc
1.0e100
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:8),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-frac @1.1-1.8 (raw "1.0e100"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-frac-f64 @1.1-1.8 (value "1e100"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.8 (type "Frac(_size)"))
~~~
