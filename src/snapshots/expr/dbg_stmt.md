# META
~~~ini
description=Debug expression stmt
type=expr
~~~
# SOURCE
~~~roc
dbg x
~~~
# EXPECTED
not_implemented - dbg_stmt.md:1:1:1:1
# PROBLEMS
NIL
# TOKENS
~~~zig
KwDbg(1:1-1:4),LowerIdent(1:5-1:6),EndOfFile(1:6-1:6),
~~~
# PARSE
~~~clojure
(e-dbg
	(e-ident @1.5-1.6 (raw "x")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Error"))
~~~
