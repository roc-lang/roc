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
(e-int @1-1-1-31 (int-var 73) (precision-var 72) (literal "999999999999999999999999999999") (value "TODO") (bound "u8") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Num(Int(*))"))
~~~