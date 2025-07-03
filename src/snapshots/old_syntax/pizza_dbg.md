# META
~~~ini
description=pizza_dbg
type=expr
~~~
# SOURCE
~~~roc
1 |> dbg
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpPizza(1:3-1:5),KwDbg(1:6-1:9),EndOfFile(1:9-1:9),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.2 (raw "1"))
~~~
# FORMATTED
~~~roc
1
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.2 (value "1"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Num(a)"))
~~~
