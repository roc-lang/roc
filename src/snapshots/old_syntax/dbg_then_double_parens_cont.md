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
# EXPECTED
NIL
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: canonicalize dbg expression
Let us know if you want to help!

# TOKENS
~~~zig
KwDbg(1:1-1:4),LowerIdent(1:5-1:6),Newline(1:1-1:1),
OpenRound(2:1-2:2),NoSpaceOpenRound(2:2-2:3),UpperIdent(2:3-2:4),Newline(1:1-1:1),
CloseRound(3:1-3:2),CloseRound(3:2-3:3),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-dbg
	(e-ident @1.5-1.6 (raw "g")))
~~~
# FORMATTED
~~~roc
dbg g
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Error"))
~~~
