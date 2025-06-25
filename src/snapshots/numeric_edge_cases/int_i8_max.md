# META
~~~ini
description=Maximum value for i8 (127)
type=expr
~~~
# SOURCE
~~~roc
127
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:4),EndOfFile(1:4-1:4),
~~~
# PARSE
~~~clojure
(e-int @1-1-1-4 (raw "127"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1-1-1-4 (num-var 74) (sign-needed "false") (bits-needed "7") (value "127") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Num(Int(*))"))
~~~