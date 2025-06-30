# META
~~~ini
description=parens_comment_tuple
type=expr
~~~
# SOURCE
~~~roc
((0#
),L)
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),NoSpaceOpenRound(1:2-1:3),Int(1:3-1:4),Newline(1:5-1:5),
CloseRound(2:1-2:2),Comma(2:2-2:3),UpperIdent(2:3-2:4),CloseRound(2:4-2:5),EndOfFile(2:5-2:5),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-2.5
	(e-tuple @1.2-2.2
		(e-int @1.3-1.4 (raw "0")))
	(e-tag @2.3-2.4 (raw "L")))
~~~
# FORMATTED
~~~roc
(
	(
		0,
	),
	L,
)
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1.1-2.5
	(elems
		(e-tuple @1.2-2.2
			(elems
				(e-int @1.3-1.4 (value "0"))))
		(e-tag @2.3-2.4 (name "L") (args "TODO"))))
~~~
# TYPES
~~~clojure
(expr @1.1-2.5 (type "(Num(*), (Num(*)), [L]*)"))
~~~
