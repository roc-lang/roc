# META
~~~ini
description=Decimal with 8 decimal places
type=expr
~~~
# SOURCE
~~~roc
3.14159265
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:11),EndOfFile(1:11-1:11),
~~~
# PARSE
~~~clojure
(e-frac @1-1-1-11 (raw "3.14159265"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec @1-1-1-11 (value "3.14159265") (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Frac(*)"))
~~~