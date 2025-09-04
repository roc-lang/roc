# META
~~~ini
description=Simple lambda application evaluation
type=expr
~~~
# SOURCE
~~~roc
(|x| x + 1)(5)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),OpBar(1:2-1:3),LowerIdent(1:3-1:4),OpBar(1:4-1:5),LowerIdent(1:6-1:7),OpPlus(1:8-1:9),Int(1:10-1:11),CloseRound(1:11-1:12),NoSpaceOpenRound(1:12-1:13),Int(1:13-1:14),CloseRound(1:14-1:15),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-apply @1.1-1.15
	(e-tuple @1.1-1.12
		(e-lambda @1.2-1.11
			(args
				(p-ident @1.3-1.4 (raw "x")))
			(e-binop @1.6-1.11 (op "+")
				(e-ident @1.6-1.7 (raw "x"))
				(e-int @1.10-1.11 (raw "1")))))
	(e-int @1.13-1.14 (raw "5")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-1.15
	(e-lambda @1.2-1.11
		(args
			(p-assign @1.3-1.4 (ident "x")))
		(e-binop @1.6-1.11 (op "add")
			(e-lookup-local @1.6-1.7
				(p-assign @1.3-1.4 (ident "x")))
			(e-int @1.10-1.11 (value "1"))))
	(e-int @1.13-1.14 (value "5")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.15 (type "Num(_size)"))
~~~
