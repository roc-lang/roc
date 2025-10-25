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
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,CloseSquare,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,NamedUnderscore,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "ConsList")
				(args
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags
					(ty (name "Nil"))
					(ty-apply
						(ty (name "Node"))
						(ty-apply
							(ty (name "ConsList"))
							(ty-var (raw "a")))))))
		(s-type-anno (name "empty")
			(ty-apply
				(ty (name "ConsList"))
				(underscore-ty-var (raw "_a"))))
		(s-decl
			(p-ident (raw "empty"))
			(e-tag (raw "ConsList.Nil")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "empty"))
		(e-nominal (nominal "ConsList")
			(e-tag (name "Nil")))
		(annotation
			(ty-apply (name "ConsList") (local)
				(ty-rigid-var (name "_a")))))
	(s-nominal-decl
		(ty-header (name "ConsList")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-tag-union
			(ty-tag-name (name "Nil"))
			(ty-tag-name (name "Node")
				(ty-apply (name "ConsList") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "ConsList(_a)")))
	(type_decls
		(nominal (type "ConsList(a)")
			(ty-header (name "ConsList")
				(ty-args
					(ty-rigid-var (name "a"))))))
	(expressions
		(expr (type "ConsList(_a)"))))
~~~
