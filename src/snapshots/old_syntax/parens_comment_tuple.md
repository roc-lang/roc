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
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),NoSpaceOpenRound(1:2-1:3),Int(1:3-1:4),Newline(1:5-1:5),
CloseRound(2:1-2:2),Comma(2:2-2:3),UpperIdent(2:3-2:4),CloseRound(2:4-2:5),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
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
		(e-int @1.3-1.4 (value "0"))
		(e-tag @2.3-2.4 (name "L"))))
~~~
# TYPES
~~~clojure
(expr @1.1-2.5 (type "(Num(*), [L]*)"))
~~~
