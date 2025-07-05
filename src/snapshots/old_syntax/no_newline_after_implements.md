# META
~~~ini
description=no_newline_after_implements
type=expr
~~~
# SOURCE
~~~roc
S implements d:J
 m#
D
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),KwImplements(1:3-1:13),LowerIdent(1:14-1:15),OpColon(1:15-1:16),UpperIdent(1:16-1:17),Newline(1:1-1:1),
LowerIdent(2:2-2:3),Newline(2:4-2:4),
UpperIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "S"))
~~~
# FORMATTED
~~~roc
S
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "S"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[S]*"))
~~~
