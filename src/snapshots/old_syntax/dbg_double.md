# META
~~~ini
description=dbg_double
type=expr
~~~
# SOURCE
~~~roc
dbg dbg g g
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize dbg expression

# TOKENS
~~~zig
KwDbg(1:1-1:4),KwDbg(1:5-1:8),LowerIdent(1:9-1:10),LowerIdent(1:11-1:12),EndOfFile(1:12-1:12),
~~~
# PARSE
~~~clojure
(e-dbg
	(e-dbg
		(e-ident @1.9-1.10 (qaul "") (raw "g"))))
~~~
# FORMATTED
~~~roc
dbg dbg g
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
