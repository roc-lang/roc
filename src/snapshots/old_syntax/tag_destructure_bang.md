# META
~~~ini
description=tag_destructure_bang
type=expr
~~~
# SOURCE
~~~roc
Config launchTheNukes! code = cfg

launchTheNukes! code
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:7),LowerIdent(1:8-1:23),LowerIdent(1:24-1:28),OpAssign(1:29-1:30),LowerIdent(1:31-1:34),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:16),LowerIdent(3:17-3:21),EndOfFile(3:21-3:21),
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
(e-tag @1.1-1.7 (ext-var 73) (name "Config") (args "TODO") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "[Config]*"))
~~~
