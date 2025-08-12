# META
~~~ini
description=Double negative unary minus operation
type=expr
~~~
# SOURCE
~~~roc
(|x| -(-x))(5)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),OpBar(1:2-1:3),LowerIdent(1:3-1:4),OpBar(1:4-1:5),OpUnaryMinus(1:6-1:7),NoSpaceOpenRound(1:7-1:8),OpUnaryMinus(1:8-1:9),LowerIdent(1:9-1:10),CloseRound(1:10-1:11),CloseRound(1:11-1:12),NoSpaceOpenRound(1:12-1:13),Int(1:13-1:14),CloseRound(1:14-1:15),EndOfFile(1:15-1:15),
~~~
# PARSE
~~~clojure
(e-apply @1.1-1.15
	(e-tuple @1.1-1.12
		(e-lambda @1.2-1.11
			(args
				(p-ident @1.3-1.4 (raw "x")))
			(unary "-"
				(e-tuple @1.7-1.11
					(unary "-"
						(e-ident @1.9-1.10 (raw "x")))))))
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
		(e-unary-minus @1.6-1.11
			(e-unary-minus @1.8-1.10
				(e-lookup-local @1.9-1.10
					(p-assign @1.3-1.4 (ident "x"))))))
	(e-int @1.13-1.14 (value "5")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.15 (type "Num(_size)"))
~~~
