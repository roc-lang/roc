# META
~~~ini
description=Example of a recursive nominal tag union with payload
type=file:NominalTagRecursivePayload.roc
~~~
# SOURCE
~~~roc
NominalTagRecursivePayload := {}

ConsList(a) := [Nil, Node(ConsList(a))]

empty : ConsList(_a)
empty = ConsList.Nil
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:27),OpColonEqual(1:28-1:30),OpenCurly(1:31-1:32),CloseCurly(1:32-1:33),
UpperIdent(3:1-3:9),NoSpaceOpenRound(3:9-3:10),LowerIdent(3:10-3:11),CloseRound(3:11-3:12),OpColonEqual(3:13-3:15),OpenSquare(3:16-3:17),UpperIdent(3:17-3:20),Comma(3:20-3:21),UpperIdent(3:22-3:26),NoSpaceOpenRound(3:26-3:27),UpperIdent(3:27-3:35),NoSpaceOpenRound(3:35-3:36),LowerIdent(3:36-3:37),CloseRound(3:37-3:38),CloseRound(3:38-3:39),CloseSquare(3:39-3:40),
LowerIdent(5:1-5:6),OpColon(5:7-5:8),UpperIdent(5:9-5:17),NoSpaceOpenRound(5:17-5:18),NamedUnderscore(5:18-5:20),CloseRound(5:20-5:21),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),UpperIdent(6:9-6:17),NoSpaceDotUpperIdent(6:17-6:21),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.21
	(type-module @1.1-1.27)
	(statements
		(s-type-decl @1.1-1.33
			(header @1.1-1.27 (name "NominalTagRecursivePayload")
				(args))
			(ty-record @1.31-1.33))
		(s-type-decl @3.1-3.40
			(header @3.1-3.12 (name "ConsList")
				(args
					(ty-var @3.10-3.11 (raw "a"))))
			(ty-tag-union @3.16-3.40
				(tags
					(ty @3.17-3.20 (name "Nil"))
					(ty-apply @3.22-3.39
						(ty @3.22-3.26 (name "Node"))
						(ty-apply @3.27-3.38
							(ty @3.27-3.35 (name "ConsList"))
							(ty-var @3.36-3.37 (raw "a")))))))
		(s-type-anno @5.1-5.21 (name "empty")
			(ty-apply @5.9-5.21
				(ty @5.9-5.17 (name "ConsList"))
				(underscore-ty-var @5.18-5.20 (raw "_a"))))
		(s-decl @6.1-6.21
			(p-ident @6.1-6.6 (raw "empty"))
			(e-tag @6.9-6.21 (raw "ConsList.Nil")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.6 (ident "empty"))
		(e-nominal @6.9-6.21 (nominal "ConsList")
			(e-tag @6.9-6.21 (name "Nil")))
		(annotation @6.1-6.6
			(declared-type
				(ty-apply @5.9-5.21 (name "ConsList") (local)
					(ty-rigid-var @5.9-5.21 (name "_a"))))))
	(s-nominal-decl @1.1-1.33
		(ty-header @1.1-1.27 (name "NominalTagRecursivePayload"))
		(ty-record @1.31-1.33))
	(s-nominal-decl @3.1-3.40
		(ty-header @3.1-3.12 (name "ConsList")
			(ty-args
				(ty-rigid-var @3.10-3.11 (name "a"))))
		(ty-tag-union @3.16-3.40
			(ty-tag-name @3.17-3.20 (name "Nil"))
			(ty-tag-name @3.22-3.39 (name "Node")
				(ty-apply @3.27-3.38 (name "ConsList") (local)
					(ty-rigid-var-lookup (ty-rigid-var @3.10-3.11 (name "a"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.6 (type "ConsList(_a)")))
	(type_decls
		(nominal @1.1-1.33 (type "NominalTagRecursivePayload")
			(ty-header @1.1-1.27 (name "NominalTagRecursivePayload")))
		(nominal @3.1-3.40 (type "ConsList(a)")
			(ty-header @3.1-3.12 (name "ConsList")
				(ty-args
					(ty-rigid-var @3.10-3.11 (name "a"))))))
	(expressions
		(expr @6.9-6.21 (type "ConsList(_a)"))))
~~~
