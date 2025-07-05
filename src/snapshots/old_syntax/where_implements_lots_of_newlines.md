# META
~~~ini
description=where_implements_lots_of_newlines
type=expr
~~~
# SOURCE
~~~roc
T:[
]#
  where
e
implements
T
e
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),OpenSquare(1:3-1:4),Newline(1:1-1:1),
CloseSquare(2:1-2:2),Newline(2:3-2:3),
KwWhere(3:3-3:8),Newline(1:1-1:1),
LowerIdent(4:1-4:2),Newline(1:1-1:1),
KwImplements(5:1-5:11),Newline(1:1-1:1),
UpperIdent(6:1-6:2),Newline(1:1-1:1),
LowerIdent(7:1-7:2),EndOfFile(7:2-7:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "T"))
~~~
# FORMATTED
~~~roc
T
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "T"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[T]*"))
~~~
