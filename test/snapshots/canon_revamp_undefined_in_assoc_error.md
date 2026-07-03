# META
~~~ini
description=Reference to a name that is never declared inside an associated block should produce a not-in-scope error
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    callsGhost : Foo
    callsGhost = ghostName

    realThing : Foo
    realThing = Whatever
}
~~~
# EXPECTED
NAME NOT IN SCOPE - canon_revamp_undefined_in_assoc_error.md:3:18:3:27
# PROBLEMS

┌───────────────────┐
│ NAME NOT IN SCOPE ├─ Nothing is named `ghostName` in this scope. ───────────┐
└┬──────────────────┘                                                         │
 │                                                                            │
 │  callsGhost = ghostName                                                    │
 │               ‾‾‾‾‾‾‾‾‾                                                    │
 └───────────────────────────── canon_revamp_undefined_in_assoc_error.md:3:18 ┘

    Is it misspelled, or is there an import missing?

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,
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
				(s-type-anno (name "callsGhost")
					(ty (name "Foo")))
				(s-decl
					(p-ident (raw "callsGhost"))
					(e-ident (raw "ghostName")))
				(s-type-anno (name "realThing")
					(ty (name "Foo")))
				(s-decl
					(p-ident (raw "realThing"))
					(e-tag (raw "Whatever")))))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	callsGhost : Foo
	callsGhost = ghostName

	realThing : Foo
	realThing = Whatever
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Foo.callsGhost"))
		(e-runtime-error (tag "ident_not_in_scope"))
		(annotation
			(ty-lookup (name "Foo") (local))))
	(d-let
		(p-assign (ident "Foo.realThing"))
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
