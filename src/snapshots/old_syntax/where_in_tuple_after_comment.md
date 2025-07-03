# META
~~~ini
description=where_in_tuple_after_comment
type=expr
~~~
# SOURCE
~~~roc
1:(*#
where e implements J)*
l
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),OpStar(1:4-1:5),Newline(1:6-1:6),
KwWhere(2:1-2:6),LowerIdent(2:7-2:8),KwImplements(2:9-2:19),UpperIdent(2:20-2:21),CloseRound(2:21-2:22),OpStar(2:22-2:23),Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:2-3:2),
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
