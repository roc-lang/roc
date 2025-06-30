# META
~~~ini
description=dbg_then_double_parens_cont
type=expr
~~~
# SOURCE
~~~roc
dbg g
((L
))#
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize dbg expression

# TOKENS
~~~zig
KwDbg(1:1-1:4),LowerIdent(1:5-1:6),Newline(1:1-1:1),
OpenRound(2:1-2:2),NoSpaceOpenRound(2:2-2:3),UpperIdent(2:3-2:4),Newline(1:1-1:1),
CloseRound(3:1-3:2),CloseRound(3:2-3:3),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-dbg
	(e-ident @1.5-1.6 (qaul "") (raw "g")))
~~~
# FORMATTED
~~~roc
dbg g
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
