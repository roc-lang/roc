# META
~~~ini
description=tag_destructure_bang_no_space
type=expr
~~~
# SOURCE
~~~roc
Config launchTheNukes!code = cfg

launchTheNukes! code
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:7),LowerIdent(1:8-1:27),OpAssign(1:28-1:29),LowerIdent(1:30-1:33),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:16),LowerIdent(3:17-3:21),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.7 (raw "Config"))
~~~
# FORMATTED
~~~roc
Config
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.7 (name "Config"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.7 (type "[Config]*"))
~~~
