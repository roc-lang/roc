# META
~~~ini
description=apply_tag
type=expr
~~~
# SOURCE
~~~roc
Whee 12 34
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:5),Int(1:6-1:8),Int(1:9-1:11),EndOfFile(1:11-1:11),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.5 (raw "Whee"))
~~~
# FORMATTED
~~~roc
Whee
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.5 (name "Whee") (args "TODO"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "[Whee]*"))
~~~
