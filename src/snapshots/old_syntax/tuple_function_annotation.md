# META
~~~ini
description=tuple_function_annotation
type=expr
~~~
# SOURCE
~~~roc
11
:(I,s
,Mw->r)l
asl
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:3),Newline(1:1-1:1),
OpColon(2:1-2:2),NoSpaceOpenRound(2:2-2:3),UpperIdent(2:3-2:4),Comma(2:4-2:5),LowerIdent(2:5-2:6),Newline(1:1-1:1),
Comma(3:1-3:2),UpperIdent(3:2-3:4),OpArrow(3:4-3:6),LowerIdent(3:6-3:7),CloseRound(3:7-3:8),LowerIdent(3:8-3:9),Newline(1:1-1:1),
LowerIdent(4:1-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.3 (raw "11"))
~~~
# FORMATTED
~~~roc
11
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.3 (value "11"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.3 (type "Num(*)"))
~~~
