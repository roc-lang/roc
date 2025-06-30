# META
~~~ini
description=where_newline_p_implements
type=expr
~~~
# SOURCE
~~~roc
2:(
)e where
p implements T
e
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),Newline(1:1-1:1),
CloseRound(2:1-2:2),LowerIdent(2:2-2:3),KwWhere(2:4-2:9),Newline(1:1-1:1),
LowerIdent(3:1-3:2),KwImplements(3:3-3:13),UpperIdent(3:14-3:15),Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.2 (raw "2"))
~~~
# FORMATTED
~~~roc
2
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.2 (value "2"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Num(*)"))
~~~
