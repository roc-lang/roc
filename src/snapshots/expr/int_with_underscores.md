# META
~~~ini
description=Integer literal with underscores
type=expr
~~~
# SOURCE
~~~roc
1_000_000
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:10),EndOfFile(1:10-1:10),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.10 (raw "1_000_000"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.10 (value "1000000"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.10 (type "Num(size)"))
~~~
