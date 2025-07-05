# META
~~~ini
description=apply_parenthetical_tag_args
type=expr
~~~
# SOURCE
~~~roc
Whee (12) (34)
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:5),OpenRound(1:6-1:7),Int(1:7-1:9),CloseRound(1:9-1:10),OpenRound(1:11-1:12),Int(1:12-1:14),CloseRound(1:14-1:15),Newline(1:1-1:1),
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
