# META
~~~ini
description=Unary minus operation on lambda parameter
type=expr
~~~
# SOURCE
~~~roc
(|x| -x)(5)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),OpBar(1:2-1:3),LowerIdent(1:3-1:4),OpBar(1:4-1:5),OpUnaryMinus(1:6-1:7),LowerIdent(1:7-1:8),CloseRound(1:8-1:9),NoSpaceOpenRound(1:9-1:10),Int(1:10-1:11),CloseRound(1:11-1:12),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-apply @1.1-1.12
	(e-tuple @1.1-1.9
		(e-lambda @1.2-1.8
			(args
				(p-ident @1.3-1.4 (raw "x")))
			(unary "-"
				(e-ident @1.7-1.8 (raw "x")))))
	(e-int @1.10-1.11 (raw "5")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-1.12
	(e-lambda @1.2-1.8
		(args
			(p-assign @1.3-1.4 (ident "x")))
		(e-unary-minus @1.6-1.8
			(e-lookup-local @1.7-1.8
				(p-assign @1.3-1.4 (ident "x")))))
	(e-num @1.10-1.11 (value "5")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.12 (type "Num(_size)"))
~~~
