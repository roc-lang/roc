# META
~~~ini
description=looks_like_implements malformed
type=expr
~~~
# SOURCE
~~~roc
N (implements) h (0):B
T
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpenRound(1:3-1:4),KwImplements(1:4-1:14),CloseRound(1:14-1:15),LowerIdent(1:16-1:17),OpenRound(1:18-1:19),Int(1:19-1:20),CloseRound(1:20-1:21),OpColon(1:21-1:22),UpperIdent(1:22-1:23),Newline(1:1-1:1),
UpperIdent(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "N"))
~~~
# FORMATTED
~~~roc
N
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "N"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[N]*"))
~~~
