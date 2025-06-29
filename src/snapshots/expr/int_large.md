# META
~~~ini
description=Large integer literal
type=expr
~~~
# SOURCE
~~~roc
999999999999999999999999999999
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:31),EndOfFile(1:31-1:31),
~~~
# PARSE
~~~clojure
(e-int @1-1-1-31 (raw "999999999999999999999999999999"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1-1-1-31 (value "999999999999999999999999999999") (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Num(*)"))
~~~
