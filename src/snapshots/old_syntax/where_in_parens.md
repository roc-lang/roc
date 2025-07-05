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
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),LowerIdent(1:4-1:5),KwWhere(1:6-1:11),Newline(1:1-1:1),
LowerIdent(2:1-2:2),Newline(1:1-1:1),
KwImplements(3:1-3:11),UpperIdent(3:12-3:13),CloseRound(3:13-3:14),UpperIdent(3:14-3:15),Newline(1:1-1:1),
LowerIdent(4:1-4:2),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
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
(e-tag @1.1-1.2 (name "L"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[L]*"))
~~~
