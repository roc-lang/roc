# META
~~~ini
description=Example of a recursive nominal tag union with payload
type=snippet
~~~
# SOURCE
~~~roc
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
UpperIdent(1:1-1:9),NoSpaceOpenRound(1:9-1:10),LowerIdent(1:10-1:11),CloseRound(1:11-1:12),OpColonEqual(1:13-1:15),OpenSquare(1:16-1:17),UpperIdent(1:17-1:20),Comma(1:20-1:21),UpperIdent(1:22-1:26),NoSpaceOpenRound(1:26-1:27),UpperIdent(1:27-1:35),NoSpaceOpenRound(1:35-1:36),LowerIdent(1:36-1:37),CloseRound(1:37-1:38),CloseRound(1:38-1:39),CloseSquare(1:39-1:40),
LowerIdent(3:1-3:6),OpColon(3:7-3:8),UpperIdent(3:9-3:17),NoSpaceOpenRound(3:17-3:18),NamedUnderscore(3:18-3:20),CloseRound(3:20-3:21),
LowerIdent(4:1-4:6),OpAssign(4:7-4:8),UpperIdent(4:9-4:17),NoSpaceDotUpperIdent(4:17-4:21),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.21
	(type-module @1.1-1.9)
	(statements
		(s-type-decl @1.1-1.40
			(header @1.1-1.12 (name "ConsList")
				(args
					(ty-var @1.10-1.11 (raw "a"))))
			(ty-tag-union @1.16-1.40
				(tags
					(ty @1.17-1.20 (name "Nil"))
					(ty-apply @1.22-1.39
						(ty @1.22-1.26 (name "Node"))
						(ty-apply @1.27-1.38
							(ty @1.27-1.35 (name "ConsList"))
							(ty-var @1.36-1.37 (raw "a")))))))
		(s-type-anno @3.1-3.21 (name "empty")
			(ty-apply @3.9-3.21
				(ty @3.9-3.17 (name "ConsList"))
				(underscore-ty-var @3.18-3.20 (raw "_a"))))
		(s-decl @4.1-4.21
			(p-ident @4.1-4.6 (raw "empty"))
			(e-tag @4.9-4.21 (raw "ConsList.Nil")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.6 (ident "empty"))
		(e-nominal @4.9-4.21 (nominal "ConsList")
			(e-tag @4.9-4.21 (name "Nil")))
		(annotation @4.1-4.6
			(declared-type
				(ty-apply @3.9-3.21 (name "ConsList") (local)
					(ty-rigid-var @3.9-3.21 (name "_a"))))))
	(s-nominal-decl @1.1-1.40
		(ty-header @1.1-1.12 (name "ConsList")
			(ty-args
				(ty-rigid-var @1.10-1.11 (name "a"))))
		(ty-tag-union @1.16-1.40
			(ty-tag-name @1.17-1.20 (name "Nil"))
			(ty-tag-name @1.22-1.39 (name "Node")
				(ty-apply @1.27-1.38 (name "ConsList") (local)
					(ty-rigid-var-lookup (ty-rigid-var @1.10-1.11 (name "a"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.6 (type "ConsList(_a)")))
	(type_decls
		(nominal @1.1-1.40 (type "ConsList(a)")
			(ty-header @1.1-1.12 (name "ConsList")
				(ty-args
					(ty-rigid-var @1.10-1.11 (name "a"))))))
	(expressions
		(expr @4.9-4.21 (type "ConsList(_a)"))))
~~~
