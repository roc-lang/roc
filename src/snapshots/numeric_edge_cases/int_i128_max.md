# META
~~~ini
description=Maximum value for i128 (170141183460469231731687303715884105727)
type=expr
~~~
# SOURCE
~~~roc
170141183460469231731687303715884105727
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:40),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.40 (raw "170141183460469231731687303715884105727"))
~~~
# FORMATTED
~~~roc
170141183460469231731687303715884105727
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.40 (value "170141183460469231731687303715884105727"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.40 (type "Num(*)"))
~~~
