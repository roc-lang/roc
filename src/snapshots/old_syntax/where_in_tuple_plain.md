# META
~~~ini
description=where_in_tuple_plain
type=expr
~~~
# SOURCE
~~~roc
1:A(*where e implements J) *
l
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpColon(1:2-1:3),UpperIdent(1:3-1:4),NoSpaceOpenRound(1:4-1:5),OpStar(1:5-1:6),KwWhere(1:6-1:11),LowerIdent(1:12-1:13),KwImplements(1:14-1:24),UpperIdent(1:25-1:26),CloseRound(1:26-1:27),OpStar(1:28-1:29),Newline(1:1-1:1),
LowerIdent(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.2 (raw "1"))
~~~
# FORMATTED
~~~roc
1
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.2 (value "1"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Num(a)"))
~~~
