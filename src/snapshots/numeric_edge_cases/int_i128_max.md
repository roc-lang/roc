# META
~~~ini
description=Maximum value for i128 (170141183460469231731687303715884105727)
type=expr
~~~
# SOURCE
~~~roc
170141183460469231731687303715884105727
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:40),EndOfFile(1:40-1:40),
~~~
# PARSE
~~~clojure
(e-int @1-1-1-40 (raw "170141183460469231731687303715884105727"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1-1-1-40 (num-var 73) (value "170141183460469231731687303715884105727") (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Num(*)"))
~~~