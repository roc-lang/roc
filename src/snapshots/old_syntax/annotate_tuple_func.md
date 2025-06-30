# META
~~~ini
description=annotate_tuple_func
type=expr
~~~
# SOURCE
~~~roc
1:(f
,ww->p)e
Mh
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),LowerIdent(1:4-1:5),Newline(1:1-1:1),
Comma(2:1-2:2),LowerIdent(2:2-2:4),OpArrow(2:4-2:6),LowerIdent(2:6-2:7),CloseRound(2:7-2:8),LowerIdent(2:8-2:9),Newline(1:1-1:1),
UpperIdent(3:1-3:3),EndOfFile(3:3-3:3),
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
(expr @1.1-1.2 (type "Num(*)"))
~~~
