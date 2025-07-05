# META
~~~ini
description=pnc_dbg_parens_comment
type=expr
~~~
# SOURCE
~~~roc
(dbg(5#
))
e
~~~
# EXPECTED
not_implemented - pnc_dbg_parens_comment.md:1:1:1:1
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),KwDbg(1:2-1:5),NoSpaceOpenRound(1:5-1:6),Int(1:6-1:7),Newline(1:8-1:8),
CloseRound(2:1-2:2),CloseRound(2:2-2:3),Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-2.3
	(e-dbg
		(e-tuple @1.5-2.2
			(e-int @1.6-1.7 (raw "5")))))
~~~
# FORMATTED
~~~roc
(
	dbg (
		5,
	),
)
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Error"))
~~~
