# META
~~~ini
description=Large integer literal
type=expr
~~~
# SOURCE
~~~roc
999999999999999999999999999999
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:31),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.31 (raw "999999999999999999999999999999"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-num @1.1-1.31 (value "999999999999999999999999999999"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.31 (type "Num(_size)"))
~~~
