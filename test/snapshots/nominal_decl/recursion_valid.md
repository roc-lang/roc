# META
~~~ini
description=Valid nominal recursion through a tag union payload is accepted
type=snippet
~~~
# SOURCE
~~~roc
ConsList := [Nil, Cons(U64, ConsList)]

t : ConsList
t = ConsList.Nil
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,CloseSquare,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "ConsList")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Nil"))
					(ty-apply
						(ty (name "Cons"))
						(ty (name "U64"))
						(ty (name "ConsList"))))))
		(s-type-anno (name "t")
			(ty (name "ConsList")))
		(s-decl
			(p-ident (raw "t"))
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
		(p-assign (ident "t"))
		(e-nominal (nominal "ConsList")
			(e-tag (name "Nil")))
		(annotation
			(ty-lookup (name "ConsList") (local))))
	(s-nominal-decl
		(ty-header (name "ConsList"))
		(ty-tag-union
			(ty-tag-name (name "Nil"))
			(ty-tag-name (name "Cons")
				(ty-lookup (name "U64") (builtin))
				(ty-lookup (name "ConsList") (local))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "ConsList")))
	(type_decls
		(nominal (type "ConsList")
			(ty-header (name "ConsList"))))
	(expressions
		(expr (type "ConsList"))))
~~~
