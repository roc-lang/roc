# META
~~~ini
description=highest_int
type=expr
~~~
# SOURCE
~~~roc
9223372036854775807
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:20),EndOfFile(1:20-1:20),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.20 (raw "9223372036854775807"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.20 (value "9223372036854775807") (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Num(*)"))
~~~
