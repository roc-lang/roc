# META
~~~ini
description=Type alias from module scope used in nominal associated items
type=snippet
~~~
# SOURCE
~~~roc
NodeKind : [Text, Other]

Elem := [].{
    kind : () -> NodeKind
    kind = || Text
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
UpperIdent,OpColonEqual,OpenSquare,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,OpenRound,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,OpBar,UpperIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "NodeKind")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Text"))
					(ty (name "Other")))))
		(s-type-decl
			(header (name "Elem")
				(args))
			(ty-tag-union
				(tags))
			(associated
				(s-type-anno (name "kind")
					(ty-fn
						(ty (name "NodeKind"))))
				(s-decl
					(p-ident (raw "kind"))
					(e-lambda
						(args)
						(e-tag (raw "Text"))))))))
~~~
# FORMATTED
~~~roc
NodeKind : [Text, Other]

Elem := [].{
	kind : () -> NodeKind
	kind = || Text
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "type_alias_in_nominal_associated.Elem.kind"))
		(e-lambda
			(args)
			(e-tag (name "Text")))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "NodeKind") (local)))))
	(s-nominal-decl
		(ty-header (name "Elem"))
		(ty-tag-union))
	(s-alias-decl
		(ty-header (name "NodeKind"))
		(ty-tag-union
			(ty-tag-name (name "Text"))
			(ty-tag-name (name "Other")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "({}) -> NodeKind")))
	(type_decls
		(nominal (type "Elem")
			(ty-header (name "Elem")))
		(alias (type "NodeKind")
			(ty-header (name "NodeKind"))))
	(expressions
		(expr (type "({}) -> NodeKind"))))
~~~
