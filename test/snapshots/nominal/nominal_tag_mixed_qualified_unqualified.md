# META
~~~ini
description=Mixing qualified and unqualified tags
type=snippet
~~~
# SOURCE
~~~roc
Color := [Red, Green, Blue]

isRed : Color -> Bool
isRed = |color| match color {
    Red => Bool.True
    Color.Green => Bool.False
    Blue => Bool.False
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
UpperIdent,NoSpaceDotUpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
UpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Color")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Red"))
					(ty (name "Green"))
					(ty (name "Blue")))))
		(s-type-anno (name "isRed")
			(ty-fn
				(ty (name "Color"))
				(ty (name "Bool"))))
		(s-decl
			(p-ident (raw "isRed"))
			(e-lambda
				(args
					(p-ident (raw "color")))
				(e-match
					(e-ident (raw "color"))
					(branches
						(branch
							(p-tag (raw "Red"))
							(e-tag (raw "Bool.True")))
						(branch
							(p-tag (raw ".Green"))
							(e-tag (raw "Bool.False")))
						(branch
							(p-tag (raw "Blue"))
							(e-tag (raw "Bool.False")))))))))
~~~
# FORMATTED
~~~roc
Color := [Red, Green, Blue]

isRed : Color -> Bool
isRed = |color| match color {
	Red => Bool.True
	Color.Green => Bool.False
	Blue => Bool.False
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "isRed"))
		(e-lambda
			(args
				(p-assign (ident "color")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "color"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-nominal-external
									(module-idx "2")
									(target-node-idx "1")
									(e-tag (name "True")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal
										(p-applied-tag))))
							(value
								(e-nominal-external
									(module-idx "2")
									(target-node-idx "1")
									(e-tag (name "False")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-nominal-external
									(module-idx "2")
									(target-node-idx "1")
									(e-tag (name "False")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Color") (local))
				(ty-lookup (name "Bool") (external (module-idx "2") (target-node-idx "1"))))))
	(s-nominal-decl
		(ty-header (name "Color"))
		(ty-tag-union
			(ty-tag-name (name "Red"))
			(ty-tag-name (name "Green"))
			(ty-tag-name (name "Blue")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Color -> Bool")))
	(type_decls
		(nominal (type "Color")
			(ty-header (name "Color"))))
	(expressions
		(expr (type "Color -> Bool"))))
~~~
