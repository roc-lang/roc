# META
~~~ini
description=dbg_stmt_multiline
type=expr
~~~
# SOURCE
~~~roc
dbg (5,
    666)

4
~~~
# EXPECTED
not_implemented - dbg_stmt_multiline.md:1:1:1:1
# PROBLEMS
NIL
# TOKENS
~~~zig
KwDbg(1:1-1:4),OpenRound(1:5-1:6),Int(1:6-1:7),Comma(1:7-1:8),Newline(1:1-1:1),
Int(2:5-2:8),CloseRound(2:8-2:9),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-dbg
	(e-tuple @1.5-2.9
		(e-int @1.6-1.7 (raw "5"))
		(e-int @2.5-2.8 (raw "666"))))
~~~
# FORMATTED
~~~roc
dbg (
	5,
	666,
)
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "not_implemented"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Error"))
~~~
