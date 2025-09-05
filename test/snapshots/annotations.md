# META
~~~ini
description=Example of a nominal tag union with a payload
type=file
~~~
# SOURCE
~~~roc
module []

f = |g, v| g(v)
~~~
# EXPECTED
TYPE MISMATCH - annotations.md:18:28:18:28
INVALID NOMINAL TAG - annotations.md:21:22:21:41
INVALID NOMINAL TAG - annotations.md:24:24:24:39
TYPE MISMATCH - annotations.md:28:35:28:35
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
LowerIdent(3:1-3:2),OpAssign(3:3-3:4),OpBar(3:5-3:6),LowerIdent(3:6-3:7),Comma(3:7-3:8),LowerIdent(3:9-3:10),OpBar(3:10-3:11),LowerIdent(3:12-3:13),NoSpaceOpenRound(3:13-3:14),LowerIdent(3:14-3:15),CloseRound(3:15-3:16),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.16
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-decl @3.1-3.16
			(p-ident @3.1-3.2 (raw "f"))
			(e-lambda @3.5-3.16
				(args
					(p-ident @3.6-3.7 (raw "g"))
					(p-ident @3.9-3.10 (raw "v")))
				(e-apply @3.12-3.16
					(e-ident @3.12-3.13 (raw "g"))
					(e-ident @3.14-3.15 (raw "v")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.2 (ident "f"))
		(e-lambda @3.5-3.16
			(args
				(p-assign @3.6-3.7 (ident "g"))
				(p-assign @3.9-3.10 (ident "v")))
			(e-call @3.12-3.16
				(e-lookup-local @3.14-3.15
					(p-assign @3.9-3.10 (ident "v")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.2 (type "a -> b, a -> b")))
	(expressions
		(expr @3.5-3.16 (type "a -> b, a -> b"))))
~~~
