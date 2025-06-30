# META
~~~ini
description=dbg_newline_apply
type=expr
~~~
# SOURCE
~~~roc
dbg

 izzb
  interfacesb
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented or doesn't have a proper error report yet: canonicalize dbg expression
Let us know if you want to help!

# TOKENS
~~~zig
KwDbg(1:1-1:4),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:2-3:6),Newline(1:1-1:1),
LowerIdent(4:3-4:14),EndOfFile(4:14-4:14),
~~~
# PARSE
~~~clojure
(e-dbg
	(e-ident @3.2-3.6 (qaul "") (raw "izzb")))
~~~
# FORMATTED
~~~roc
dbg

	izzb
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
