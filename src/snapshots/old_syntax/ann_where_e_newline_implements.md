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
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),OpenSquare(1:3-1:4),Newline(1:1-1:1),
CloseSquare(2:1-2:2),KwWhere(2:2-2:7),LowerIdent(2:8-2:9),Newline(1:1-1:1),
KwImplements(3:3-3:13),UpperIdent(3:14-3:15),Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:2-4:2),
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
(e-tag @1.1-1.2 (name "J") (args "TODO"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[J]a"))
~~~
