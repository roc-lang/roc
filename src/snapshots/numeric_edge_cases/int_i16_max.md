# META
~~~ini
description=Maximum value for i16 (32767)
type=expr
~~~
# SOURCE
~~~roc
32767
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:6),EndOfFile(1:6-1:6),
~~~
# PARSE
~~~clojure
(e-int @1-1-1-6 (raw "32767"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1-1-1-6 (num-var 73) (value "32767") (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Num(*)"))
~~~