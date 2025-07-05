# META
~~~ini
description=dbg_pnc_zero_args
type=expr
~~~
# SOURCE
~~~roc
dbg()
d
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: canonicalize dbg expression
Let us know if you want to help!

# TOKENS
~~~zig
KwDbg(1:1-1:4),NoSpaceOpenRound(1:4-1:5),CloseRound(1:5-1:6),Newline(1:1-1:1),
LowerIdent(2:1-2:2),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-dbg
	(e-tuple @1.4-1.6))
~~~
# FORMATTED
~~~roc
dbg ()
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Error"))
~~~
