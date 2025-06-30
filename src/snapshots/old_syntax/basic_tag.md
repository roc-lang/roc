# META
~~~ini
description=basic_tag
type=expr
~~~
# SOURCE
~~~roc
Whee
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.5 (raw "Whee"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.5 (ext-var 73) (name "Whee") (args "TODO") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "[Whee]*"))
~~~
