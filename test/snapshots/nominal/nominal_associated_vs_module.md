# META
~~~ini
description=Qualified names should be checked locally before treating as module references
type=file
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [Something]
}

# This should resolve to the local Foo.Bar, not try to import from a Foo module
useBar : Foo.Bar
useBar = Something
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Whatever"))))
			(associated
				(s-type-decl
					(header (name "Bar")
						(args))
					(ty-tag-union
						(tags
							(ty (name "Something")))))))
		(s-type-anno (name "useBar")
			(ty (name "Foo.Bar")))
		(s-decl
			(p-ident (raw "useBar"))
			(e-tag (raw "Something")))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [Something]
}

# This should resolve to the local Foo.Bar, not try to import from a Foo module
useBar : Foo.Bar
useBar = Something
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "useBar"))
		(e-tag (name "Something"))
		(annotation
			(ty-lookup (name "Foo.Bar") (local))))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "Whatever"))))
	(s-nominal-decl
		(ty-header (name "nominal_associated_vs_module.Foo.Bar"))
		(ty-tag-union
			(ty-tag-name (name "Something")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "nominal_associated_vs_module.Foo.Bar")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "nominal_associated_vs_module.Foo.Bar")
			(ty-header (name "nominal_associated_vs_module.Foo.Bar"))))
	(expressions
		(expr (type "nominal_associated_vs_module.Foo.Bar"))))
~~~
