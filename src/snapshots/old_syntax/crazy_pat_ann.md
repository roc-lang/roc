# META
~~~ini
description=crazy_pat_ann
type=expr
~~~
# SOURCE
~~~roc
((((0)
)#
))f:f
t
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),NoSpaceOpenRound(1:2-1:3),NoSpaceOpenRound(1:3-1:4),NoSpaceOpenRound(1:4-1:5),Int(1:5-1:6),CloseRound(1:6-1:7),Newline(1:1-1:1),
CloseRound(2:1-2:2),Newline(2:3-2:3),
CloseRound(3:1-3:2),CloseRound(3:2-3:3),LowerIdent(3:3-3:4),OpColon(3:4-3:5),LowerIdent(3:5-3:6),Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-3.3
	(e-tuple @1.2-3.2
		(e-tuple @1.3-2.2
			(e-tuple @1.4-1.7
				(e-int @1.5-1.6 (raw "0"))))))
~~~
# FORMATTED
~~~roc
(
	(
		(
			(0),
		),
	),
)
~~~
# CANONICALIZE
~~~clojure
(e-int @1.5-1.6 (value "0"))
~~~
# TYPES
~~~clojure
(expr @1.5-1.6 (type "Num(*)"))
~~~
