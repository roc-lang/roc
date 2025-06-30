# META
~~~ini
description=Dec literal with negative exponent scientific notation
type=expr
~~~
# SOURCE
~~~roc
1.23456789012345678e-10
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Float(1:1-1:24),EndOfFile(1:24-1:24),
~~~
# PARSE
~~~clojure
(e-frac @1.1-1.24 (raw "1.23456789012345678e-10"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec @1.1-1.24 (value "0.000000000123456789"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.24 (type "Frac(*)"))
~~~
