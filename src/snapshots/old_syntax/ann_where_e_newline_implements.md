# META
~~~ini
description=ann_where_e_newline_implements
type=expr
~~~
# SOURCE
~~~roc
J:[
]where e
  implements T
i
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),OpenSquare(1:3-1:4),Newline(1:1-1:1),
CloseSquare(2:1-2:2),KwWhere(2:2-2:7),LowerIdent(2:8-2:9),Newline(1:1-1:1),
KwImplements(3:3-3:13),UpperIdent(3:14-3:15),Newline(1:1-1:1),
LowerIdent(4:1-4:2),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "J"))
~~~
# FORMATTED
~~~roc
J
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "J"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[J]*"))
~~~
