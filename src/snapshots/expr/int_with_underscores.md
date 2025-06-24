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
(e-int @1-1-1-10 (int-var 73) (precision-var 72) (literal "1_000_000") (value "TODO") (bound "u8") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Num(Int(*))"))
~~~