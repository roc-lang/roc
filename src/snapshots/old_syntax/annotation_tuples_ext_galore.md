# META
~~~ini
description=annotation_tuples_ext_galore
type=expr
~~~
# SOURCE
~~~roc
1:(()(n#
))(n)
l
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),NoSpaceOpenRound(1:4-1:5),CloseRound(1:5-1:6),NoSpaceOpenRound(1:6-1:7),LowerIdent(1:7-1:8),Newline(1:9-1:9),
CloseRound(2:1-2:2),CloseRound(2:2-2:3),NoSpaceOpenRound(2:3-2:4),LowerIdent(2:4-2:5),CloseRound(2:5-2:6),Newline(1:1-1:1),
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
