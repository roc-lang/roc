# META
~~~ini
description=Caret operator (^) parsing
type=expr
~~~
# SOURCE
~~~roc
2 ^ 3 ^ 4
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int,OpCaret,Int,OpCaret,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-int (raw "2"))
~~~
# FORMATTED
~~~roc
2
~~~
# CANONICALIZE
~~~clojure
(e-num (value "2"))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
