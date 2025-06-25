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
(e-int @1-1-1-31 (num-var 74) (sign-needed "false") (bits-needed "65_to_127") (value "999999999999999999999999999999") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Num(Int(*))"))
~~~