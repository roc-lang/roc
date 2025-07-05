# META
~~~ini
description=dbg_double_newline
type=expr
~~~
# SOURCE
~~~roc
dbg dbg
 a g
~~~
# EXPECTED
not_implemented - dbg_double_newline.md:1:1:1:1
# PROBLEMS
NIL
# TOKENS
~~~zig
KwDbg(1:1-1:4),KwDbg(1:5-1:8),Newline(1:1-1:1),
LowerIdent(2:2-2:3),LowerIdent(2:4-2:5),EndOfFile(2:5-2:5),
~~~
# PARSE
~~~clojure
(e-dbg
	(e-dbg
		(e-ident @2.2-2.3 (raw "a"))))
~~~
# FORMATTED
~~~roc
dbg dbg
	a
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Error"))
~~~
