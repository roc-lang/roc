# META
~~~ini
description=dbg_pnc_a_over_a
type=expr
~~~
# SOURCE
~~~roc
dbg(a/a)
d
~~~
# EXPECTED
not_implemented - dbg_pnc_a_over_a.md:1:1:1:1
# PROBLEMS
NIL
# TOKENS
~~~zig
KwDbg(1:1-1:4),NoSpaceOpenRound(1:4-1:5),LowerIdent(1:5-1:6),OpSlash(1:6-1:7),LowerIdent(1:7-1:8),CloseRound(1:8-1:9),Newline(1:1-1:1),
LowerIdent(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-dbg
	(e-tuple @1.4-1.9
		(e-binop @1.5-1.9 (op "/")
			(e-ident @1.5-1.6 (raw "a"))
			(e-ident @1.7-1.8 (raw "a")))))
~~~
# FORMATTED
~~~roc
dbg (a / a)
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Error"))
~~~
