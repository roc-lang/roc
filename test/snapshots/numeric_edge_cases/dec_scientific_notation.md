# META
~~~ini
description=Dec literal with scientific notation
type=expr
~~~
# SOURCE
~~~roc
1.23456789012345678e10
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
(e-frac (raw "1.23456789012345678e10"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec (value "12345678901.234568"))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
