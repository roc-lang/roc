# META
~~~ini
description=where_in_parens
type=expr
~~~
# SOURCE
~~~roc
L:(l where
e
implements Z)I
s
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),LowerIdent(1:4-1:5),KwWhere(1:6-1:11),Newline(1:1-1:1),
LowerIdent(2:1-2:2),Newline(1:1-1:1),
KwImplements(3:1-3:11),UpperIdent(3:12-3:13),CloseRound(3:13-3:14),UpperIdent(3:14-3:15),Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "L"))
~~~
# FORMATTED
~~~roc
L
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "L") (args "TODO"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[L]a"))
~~~
