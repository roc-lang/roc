# META
~~~ini
description=Pizza operator (|>) parsing
type=expr
~~~
# SOURCE
~~~roc
1 |> add 2 |> mul 3
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int,OpPizza,LowerIdent,Int,OpPizza,LowerIdent,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-int (raw "1"))
~~~
# FORMATTED
~~~roc
1
~~~
# CANONICALIZE
~~~clojure
(e-num (value "1"))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
