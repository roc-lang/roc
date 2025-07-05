# META
~~~ini
description=dbg_double
type=expr
~~~
# SOURCE
~~~roc
dbg dbg g g
~~~
# EXPECTED
not_implemented - dbg_double.md:1:1:1:1
# PROBLEMS
NIL
# TOKENS
~~~zig
KwDbg(1:1-1:4),KwDbg(1:5-1:8),LowerIdent(1:9-1:10),LowerIdent(1:11-1:12),EndOfFile(1:12-1:12),
~~~
# PARSE
~~~clojure
(e-dbg
	(e-dbg
		(e-ident @1.9-1.10 (raw "g"))))
~~~
# FORMATTED
~~~roc
dbg dbg g
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Error"))
~~~
