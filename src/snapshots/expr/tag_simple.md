# META
~~~ini
description=Simple tag literal
type=expr
~~~
# SOURCE
~~~roc
Ok
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:3),EndOfFile(1:3-1:3),
~~~
# PARSE
~~~clojure
(e-tag @1-1-1-3 (raw "Ok"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tag @1-1-1-3 (ext-var 0) (name "Ok") (args "TODO") (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "[Ok, * *]"))
~~~