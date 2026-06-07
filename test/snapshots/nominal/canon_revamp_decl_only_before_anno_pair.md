# META
~~~ini
description=Decl-only (no anno) before a matching anno+decl pair, where the decl-only refs the pair
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    useful = labeled

    labeled : Foo
    labeled = Whatever
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,
CloseCurly,
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
				(s-decl
					(p-ident (raw "useful"))
					(e-ident (raw "labeled")))
				(s-type-anno (name "labeled")
					(ty (name "Foo")))
				(s-decl
					(p-ident (raw "labeled"))
					(e-tag (raw "Whatever")))))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	useful = labeled

	labeled : Foo
	labeled = Whatever
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Foo.useful"))
		(e-lookup-local
			(p-assign (ident "Foo.labeled"))))
	(d-let
		(p-assign (ident "Foo.labeled"))
		(e-tag (name "Whatever"))
		(annotation
			(ty-lookup (name "Foo") (local))))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "Whatever")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Foo"))
		(patt (type "Foo")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo"))))
	(expressions
		(expr (type "Foo"))
		(expr (type "Foo"))))
~~~
