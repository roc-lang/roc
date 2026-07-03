# META
~~~ini
description=Method forward-references an anno-only sibling within the same associated block
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    callMe : Foo
    callMe = absent

    absent : Foo
}
~~~
# EXPECTED
NAME NOT IN SCOPE - canon_revamp_forward_ref_to_anno_only.md:3:14:3:20
DECLARATION HAS NO VALUE - canon_revamp_forward_ref_to_anno_only.md:5:5:5:17
# PROBLEMS

┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `absent` in this scope. ──────────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  callMe = absent                                                           │
 │           ‾‾‾‾‾‾                                                           │
 └───────────────────────────── canon_revamp_forward_ref_to_anno_only.md:3:14 ┘

    Is it misspelled, or is there an import missing?


┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  absent : Foo                                                              │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾                                                              │
 └────────────────────────────── canon_revamp_forward_ref_to_anno_only.md:5:5 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpColon,UpperIdent,
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
				(s-type-anno (name "callMe")
					(ty (name "Foo")))
				(s-decl
					(p-ident (raw "callMe"))
					(e-ident (raw "absent")))
				(s-type-anno (name "absent")
					(ty (name "Foo")))))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	callMe : Foo
	callMe = absent

	absent : Foo
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Foo.callMe"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(annotation
			(ty-lookup (name "Foo") (local))))
	(d-let
		(p-assign (ident "Foo.absent"))
		(e-anno-only)
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
