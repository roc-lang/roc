# META
~~~ini
description=pnc_parens_apply_etc
type=expr
~~~
# SOURCE
~~~roc
(
3)():B
(z)
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),Newline(1:1-1:1),
Int(2:1-2:2),CloseRound(2:2-2:3),NoSpaceOpenRound(2:3-2:4),CloseRound(2:4-2:5),OpColon(2:5-2:6),UpperIdent(2:6-2:7),Newline(1:1-1:1),
OpenRound(3:1-3:2),LowerIdent(3:2-3:3),CloseRound(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-apply @1.1-2.5
	(e-tuple @1.1-2.3
		(e-int @2.1-2.2 (raw "3"))))
~~~
# FORMATTED
~~~roc
(
	3,
)()
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-2.5 (id 76)
	(e-tuple @1.1-2.3
		(elems
			(e-int @2.1-2.2 (value "3")))))
~~~
# TYPES
~~~clojure
(expr (id 76) (type "*"))
~~~
