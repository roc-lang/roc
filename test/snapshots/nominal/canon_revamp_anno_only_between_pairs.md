# META
~~~ini
description=Anno-only def sandwiched between two matching anno+decl pairs - verifies PostAnno doesn't double-register the surrounding pairs and correctly registers the anno-only
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    first : Foo
    first = Whatever

    middle : Foo

    last : Foo
    last = Whatever
}
~~~
# EXPECTED
DECLARATION HAS NO VALUE - canon_revamp_anno_only_between_pairs.md:5:5:5:17
# PROBLEMS

┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  middle : Foo                                                              │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾                                                              │
 └─────────────────────────────── canon_revamp_anno_only_between_pairs.md:5:5 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,
LowerIdent,OpColon,UpperIdent,
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
				(s-type-anno (name "first")
					(ty (name "Foo")))
				(s-decl
					(p-ident (raw "first"))
					(e-tag (raw "Whatever")))
				(s-type-anno (name "middle")
					(ty (name "Foo")))
				(s-type-anno (name "last")
					(ty (name "Foo")))
				(s-decl
					(p-ident (raw "last"))
					(e-tag (raw "Whatever")))))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	first : Foo
	first = Whatever

	middle : Foo

	last : Foo
	last = Whatever
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Foo.first"))
		(e-tag (name "Whatever"))
		(annotation
			(ty-lookup (name "Foo") (local))))
	(d-let
		(p-assign (ident "Foo.middle"))
		(e-anno-only)
		(annotation
			(ty-lookup (name "Foo") (local))))
	(d-let
		(p-assign (ident "Foo.last"))
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
		(patt (type "Foo"))
		(patt (type "Foo")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo"))))
	(expressions
		(expr (type "Foo"))
		(expr (type "Foo"))
		(expr (type "Foo"))))
~~~
