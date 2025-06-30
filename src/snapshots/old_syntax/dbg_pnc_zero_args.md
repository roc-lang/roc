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
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: canonicalize dbg expression
Let us know if you want to help!

# TOKENS
~~~zig
KwDbg(1:1-1:4),NoSpaceOpenRound(1:4-1:5),CloseRound(1:5-1:6),Newline(1:1-1:1),
LowerIdent(2:1-2:2),EndOfFile(2:2-2:2),
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
(e-runtime-error (tag "not_implemented") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
