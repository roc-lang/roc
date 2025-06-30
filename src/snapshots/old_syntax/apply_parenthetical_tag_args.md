# META
~~~ini
description=apply_parenthetical_tag_args
type=expr
~~~
# SOURCE
~~~roc
Whee (12) (34)
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:5),OpenRound(1:6-1:7),Int(1:7-1:9),CloseRound(1:9-1:10),OpenRound(1:11-1:12),Int(1:12-1:14),CloseRound(1:14-1:15),EndOfFile(1:15-1:15),
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
(e-tag @1.1-1.5 (ext-var 73) (name "Whee") (args "TODO") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "[Whee]*"))
~~~
