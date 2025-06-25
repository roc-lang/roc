# META
~~~ini
description=Integer literal with underscores
type=expr
~~~
# SOURCE
~~~roc
1_000_000
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:10),EndOfFile(1:10-1:10),
~~~
# PARSE
~~~clojure
(e-int @1-1-1-10 (raw "1_000_000"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1-1-1-10 (num-var 74) (sign-needed "false") (bits-needed "17_to_31") (value "1000000") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Num(Int(*))"))
~~~