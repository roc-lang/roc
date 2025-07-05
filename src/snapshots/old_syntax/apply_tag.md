# META
~~~ini
description=apply_tag
type=expr
~~~
# SOURCE
~~~roc
Whee 12 34
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:5),Int(1:6-1:8),Int(1:9-1:11),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
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
(e-tag @1.1-1.5 (name "Whee"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "[Whee]*"))
~~~
