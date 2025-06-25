# META
~~~ini
description=Maximum value for i64 (9223372036854775807)
type=expr
~~~
# SOURCE
~~~roc
9223372036854775807
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:20),EndOfFile(1:20-1:20),
~~~
# PARSE
~~~clojure
(e-int @1-1-1-20 (raw "9223372036854775807"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-int @1-1-1-20 (num-var 74) (sign-needed "false") (bits-needed "33_to_63") (value "9223372036854775807") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Num(Int(*))"))
~~~