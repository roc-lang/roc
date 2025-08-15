# META
~~~ini
description=Boolean closure type checking - should have no errors
type=expr
~~~
# SOURCE
~~~roc
(|x| !x)(True)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),OpBar(1:2-1:3),LowerIdent(1:3-1:4),OpBar(1:4-1:5),OpBang(1:6-1:7),LowerIdent(1:7-1:8),CloseRound(1:8-1:9),NoSpaceOpenRound(1:9-1:10),UpperIdent(1:10-1:14),CloseRound(1:14-1:15),EndOfFile(1:15-1:15),
~~~
# PARSE
~~~clojure
(e-apply @1.1-1.15
	(e-tuple @1.1-1.9
		(e-lambda @1.2-1.8
			(args
				(p-ident @1.3-1.4 (raw "x")))
			(unary "!"
				(e-ident @1.7-1.8 (raw "x")))))
	(e-tag @1.10-1.14 (raw "True")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-1.15
	(e-lambda @1.2-1.8
		(args
			(p-assign @1.3-1.4 (ident "x")))
		(e-unary-not @1.6-1.8
			(e-lookup-local @1.7-1.8
				(p-assign @1.3-1.4 (ident "x")))))
	(e-nominal @1.10-1.14 (nominal "Bool")
		(e-tag @1.10-1.14 (name "True"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.15 (type "Bool"))
~~~
