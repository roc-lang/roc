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
Float(1:1-1:23),EndOfFile(1:23-1:23),
~~~
# PARSE
~~~clojure
(e-frac @1.1-1.23 (raw "1.23456789012345678e10"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-frac-dec @1.1-1.23 (value "1.2345678901234568e10"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.23 (type "Frac(size)"))
~~~
