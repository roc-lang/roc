# META
~~~ini
description=A public alias renames a private nominal type only when it applies it to exactly its own parameters in order, so a concrete argument, a repeated parameter, swapped parameters, or a type built around the nominal type still warn
type=file:Api.roc
~~~
# SOURCE
~~~roc
Api := {
    concrete : Api.Concrete(U64),
    repeated : Api.Repeated(U64),
    swapped : Api.Swapped(U64, Str),
    wrapped : Api.Wrapped,
}.{
    Concrete(a) : Pair(a, Str)
    Repeated(a) : Pair(a, a)
    Swapped(a, b) : Pair(b, a)
    Wrapped : List(Single)
}
Pair(a, b) := { first : a, second : b }
Single := [One]
~~~
# EXPECTED
PRIVATE TYPE IN EXPOSED FIELD - type_mod_public_alias_that_does_not_rename_still_warns.md:2:16:2:33
PRIVATE TYPE IN EXPOSED FIELD - type_mod_public_alias_that_does_not_rename_still_warns.md:3:16:3:33
PRIVATE TYPE IN EXPOSED FIELD - type_mod_public_alias_that_does_not_rename_still_warns.md:4:15:4:36
PRIVATE TYPE IN EXPOSED FIELD - type_mod_public_alias_that_does_not_rename_still_warns.md:5:15:5:26
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Private Type In Exposed Field")
		(region (start 2 16) (end 2 33))
		(headline
			(reflow "The ")
			(annotated symbol-unqualified "concrete")
			(reflow " field of ")
			(annotated code "Api")
			(reflow " refers to ")
			(annotated code "Pair")
			(reflow ", but ")
			(annotated code "Pair")
			(reflow " is private to this mod."))
		(document
			(reflow "Other mods can see this field because ")
			(annotated type "Api")
			(reflow " is exposed and not opaque, but they cannot name this private type.")
			(line-break)
			(line-break)
			(source-region (file "type_mod_public_alias_that_does_not_rename_still_warns.md") (start 2 16) (end 2 33) (annotation warning) (line-text "    concrete : Api.Concrete(U64),"))
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " Expose the referenced type, make ")
			(annotated type "Api")
			(reflow " opaque with ")
			(annotated code "::")
			(reflow ", or move the type into ")
			(annotated type "Api")
			(reflow "'s associated block.")))
	(report
		(severity warning)
		(title "Private Type In Exposed Field")
		(region (start 3 16) (end 3 33))
		(headline
			(reflow "The ")
			(annotated symbol-unqualified "repeated")
			(reflow " field of ")
			(annotated code "Api")
			(reflow " refers to ")
			(annotated code "Pair")
			(reflow ", but ")
			(annotated code "Pair")
			(reflow " is private to this mod."))
		(document
			(reflow "Other mods can see this field because ")
			(annotated type "Api")
			(reflow " is exposed and not opaque, but they cannot name this private type.")
			(line-break)
			(line-break)
			(source-region (file "type_mod_public_alias_that_does_not_rename_still_warns.md") (start 3 16) (end 3 33) (annotation warning) (line-text "    repeated : Api.Repeated(U64),"))
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " Expose the referenced type, make ")
			(annotated type "Api")
			(reflow " opaque with ")
			(annotated code "::")
			(reflow ", or move the type into ")
			(annotated type "Api")
			(reflow "'s associated block.")))
	(report
		(severity warning)
		(title "Private Type In Exposed Field")
		(region (start 4 15) (end 4 36))
		(headline
			(reflow "The ")
			(annotated symbol-unqualified "swapped")
			(reflow " field of ")
			(annotated code "Api")
			(reflow " refers to ")
			(annotated code "Pair")
			(reflow ", but ")
			(annotated code "Pair")
			(reflow " is private to this mod."))
		(document
			(reflow "Other mods can see this field because ")
			(annotated type "Api")
			(reflow " is exposed and not opaque, but they cannot name this private type.")
			(line-break)
			(line-break)
			(source-region (file "type_mod_public_alias_that_does_not_rename_still_warns.md") (start 4 15) (end 4 36) (annotation warning) (line-text "    swapped : Api.Swapped(U64, Str),"))
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " Expose the referenced type, make ")
			(annotated type "Api")
			(reflow " opaque with ")
			(annotated code "::")
			(reflow ", or move the type into ")
			(annotated type "Api")
			(reflow "'s associated block.")))
	(report
		(severity warning)
		(title "Private Type In Exposed Field")
		(region (start 5 15) (end 5 26))
		(headline
			(reflow "The ")
			(annotated symbol-unqualified "wrapped")
			(reflow " field of ")
			(annotated code "Api")
			(reflow " refers to ")
			(annotated code "Single")
			(reflow ", but ")
			(annotated code "Single")
			(reflow " is private to this mod."))
		(document
			(reflow "Other mods can see this field because ")
			(annotated type "Api")
			(reflow " is exposed and not opaque, but they cannot name this private type.")
			(line-break)
			(line-break)
			(source-region (file "type_mod_public_alias_that_does_not_rename_still_warns.md") (start 5 15) (end 5 26) (annotation warning) (line-text "    wrapped : Api.Wrapped,"))
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " Expose the referenced type, make ")
			(annotated type "Api")
			(reflow " opaque with ")
			(annotated code "::")
			(reflow ", or move the type into ")
			(annotated type "Api")
			(reflow "'s associated block."))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,Comma,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,Comma,
CloseCurly,Dot,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,UpperIdent,CloseRound,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
UpperIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColonEqual,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,LowerIdent,CloseCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Api")
				(args))
			(ty-record
				(anno-record-field (name "concrete")
					(ty-apply
						(ty (name "Api.Concrete"))
						(ty (name "U64"))))
				(anno-record-field (name "repeated")
					(ty-apply
						(ty (name "Api.Repeated"))
						(ty (name "U64"))))
				(anno-record-field (name "swapped")
					(ty-apply
						(ty (name "Api.Swapped"))
						(ty (name "U64"))
						(ty (name "Str"))))
				(anno-record-field (name "wrapped")
					(ty (name "Api.Wrapped"))))
			(associated
				(s-type-decl
					(header (name "Concrete")
						(args
							(ty-var (raw "a"))))
					(ty-apply
						(ty (name "Pair"))
						(ty-var (raw "a"))
						(ty (name "Str"))))
				(s-type-decl
					(header (name "Repeated")
						(args
							(ty-var (raw "a"))))
					(ty-apply
						(ty (name "Pair"))
						(ty-var (raw "a"))
						(ty-var (raw "a"))))
				(s-type-decl
					(header (name "Swapped")
						(args
							(ty-var (raw "a"))
							(ty-var (raw "b"))))
					(ty-apply
						(ty (name "Pair"))
						(ty-var (raw "b"))
						(ty-var (raw "a"))))
				(s-type-decl
					(header (name "Wrapped")
						(args))
					(ty-apply
						(ty (name "List"))
						(ty (name "Single"))))))
		(s-type-decl
			(header (name "Pair")
				(args
					(ty-var (raw "a"))
					(ty-var (raw "b"))))
			(ty-record
				(anno-record-field (name "first")
					(ty-var (raw "a")))
				(anno-record-field (name "second")
					(ty-var (raw "b")))))
		(s-type-decl
			(header (name "Single")
				(args))
			(ty-tag-union
				(tags
					(ty (name "One")))))))
~~~
# FORMATTED
~~~roc
Api := {
	concrete : Api.Concrete(U64),
	repeated : Api.Repeated(U64),
	swapped : Api.Swapped(U64, Str),
	wrapped : Api.Wrapped,
}.{
	Concrete(a) : Pair(a, Str)
	Repeated(a) : Pair(a, a)
	Swapped(a, b) : Pair(b, a)
	Wrapped : List(Single)
}

Pair(a, b) := { first : a, second : b }

Single := [One]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl
		(ty-header (name "Api"))
		(ty-record
			(field (field "concrete")
				(ty-apply (name "Api.Concrete") (local)
					(ty-lookup (name "U64") (builtin))))
			(field (field "repeated")
				(ty-apply (name "Api.Repeated") (local)
					(ty-lookup (name "U64") (builtin))))
			(field (field "swapped")
				(ty-apply (name "Api.Swapped") (local)
					(ty-lookup (name "U64") (builtin))
					(ty-lookup (name "Str") (builtin))))
			(field (field "wrapped")
				(ty-lookup (name "Api.Wrapped") (local)))))
	(s-alias-decl
		(ty-header (name "Api.Concrete")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-apply (name "Pair") (local)
			(ty-rigid-var-lookup (ty-rigid-var (name "a")))
			(ty-lookup (name "Str") (builtin))))
	(s-alias-decl
		(ty-header (name "Api.Repeated")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-apply (name "Pair") (local)
			(ty-rigid-var-lookup (ty-rigid-var (name "a")))
			(ty-rigid-var-lookup (ty-rigid-var (name "a")))))
	(s-alias-decl
		(ty-header (name "Api.Swapped")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))))
		(ty-apply (name "Pair") (local)
			(ty-rigid-var-lookup (ty-rigid-var (name "b")))
			(ty-rigid-var-lookup (ty-rigid-var (name "a")))))
	(s-alias-decl
		(ty-header (name "Api.Wrapped"))
		(ty-apply (name "List") (builtin)
			(ty-lookup (name "Single") (local))))
	(s-nominal-decl
		(ty-header (name "Pair")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))))
		(ty-record
			(field (field "first")
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
			(field (field "second")
				(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))
	(s-nominal-decl
		(ty-header (name "Single"))
		(ty-tag-union
			(ty-tag-name (name "One")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal (type "Api")
			(ty-header (name "Api")))
		(alias (type "Api.Concrete(a)")
			(ty-header (name "Api.Concrete")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Api.Repeated(a)")
			(ty-header (name "Api.Repeated")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Api.Swapped(a, b)")
			(ty-header (name "Api.Swapped")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))))
		(alias (type "Api.Wrapped")
			(ty-header (name "Api.Wrapped")))
		(nominal (type "Pair(a, b)")
			(ty-header (name "Pair")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b")))))
		(nominal (type "Single")
			(ty-header (name "Single"))))
	(expressions))
~~~
