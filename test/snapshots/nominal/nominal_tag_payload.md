# META
~~~ini
description=Example of a nominal tag union with a payload
type=file
~~~
# SOURCE
~~~roc
Maybe(a) := [Some(a), None]

some1 : a -> Maybe(a)
some1 = |a| Maybe.Some(a)

none1 : Maybe(_a)
none1 = Maybe.None

some2 = |a| Maybe.Some(a)

none2 = Maybe.None
~~~
# EXPECTED
TYPE MODULE MISSING MATCHING TYPE - nominal_tag_payload.md:1:1:11:19
# PROBLEMS
**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `nominal_tag_payload.roc`, but no top-level type declaration named `nominal_tag_payload` was found.

Add either:
`nominal_tag_payload := ...` (nominal type)
or:
`nominal_tag_payload : ...` (type alias)
**nominal_tag_payload.md:1:1:11:19:**
```roc
Maybe(a) := [Some(a), None]

some1 : a -> Maybe(a)
some1 = |a| Maybe.Some(a)

none1 : Maybe(_a)
none1 = Maybe.None

some2 = |a| Maybe.Some(a)

none2 = Maybe.None
```


# TOKENS
~~~zig
UpperIdent(1:1-1:6),NoSpaceOpenRound(1:6-1:7),LowerIdent(1:7-1:8),CloseRound(1:8-1:9),OpColonEqual(1:10-1:12),OpenSquare(1:13-1:14),UpperIdent(1:14-1:18),NoSpaceOpenRound(1:18-1:19),LowerIdent(1:19-1:20),CloseRound(1:20-1:21),Comma(1:21-1:22),UpperIdent(1:23-1:27),CloseSquare(1:27-1:28),
LowerIdent(3:1-3:6),OpColon(3:7-3:8),LowerIdent(3:9-3:10),OpArrow(3:11-3:13),UpperIdent(3:14-3:19),NoSpaceOpenRound(3:19-3:20),LowerIdent(3:20-3:21),CloseRound(3:21-3:22),
LowerIdent(4:1-4:6),OpAssign(4:7-4:8),OpBar(4:9-4:10),LowerIdent(4:10-4:11),OpBar(4:11-4:12),UpperIdent(4:13-4:18),NoSpaceDotUpperIdent(4:18-4:23),NoSpaceOpenRound(4:23-4:24),LowerIdent(4:24-4:25),CloseRound(4:25-4:26),
LowerIdent(6:1-6:6),OpColon(6:7-6:8),UpperIdent(6:9-6:14),NoSpaceOpenRound(6:14-6:15),NamedUnderscore(6:15-6:17),CloseRound(6:17-6:18),
LowerIdent(7:1-7:6),OpAssign(7:7-7:8),UpperIdent(7:9-7:14),NoSpaceDotUpperIdent(7:14-7:19),
LowerIdent(9:1-9:6),OpAssign(9:7-9:8),OpBar(9:9-9:10),LowerIdent(9:10-9:11),OpBar(9:11-9:12),UpperIdent(9:13-9:18),NoSpaceDotUpperIdent(9:18-9:23),NoSpaceOpenRound(9:23-9:24),LowerIdent(9:24-9:25),CloseRound(9:25-9:26),
LowerIdent(11:1-11:6),OpAssign(11:7-11:8),UpperIdent(11:9-11:14),NoSpaceDotUpperIdent(11:14-11:19),
EndOfFile(12:1-12:1),
~~~
# PARSE
~~~clojure
(file @1.1-11.19
	(type-module @1.1-1.6)
	(statements
		(s-type-decl @1.1-1.28
			(header @1.1-1.9 (name "Maybe")
				(args
					(ty-var @1.7-1.8 (raw "a"))))
			(ty-tag-union @1.13-1.28
				(tags
					(ty-apply @1.14-1.21
						(ty @1.14-1.18 (name "Some"))
						(ty-var @1.19-1.20 (raw "a")))
					(ty @1.23-1.27 (name "None")))))
		(s-type-anno @3.1-3.22 (name "some1")
			(ty-fn @3.9-3.22
				(ty-var @3.9-3.10 (raw "a"))
				(ty-apply @3.14-3.22
					(ty @3.14-3.19 (name "Maybe"))
					(ty-var @3.20-3.21 (raw "a")))))
		(s-decl @4.1-4.26
			(p-ident @4.1-4.6 (raw "some1"))
			(e-lambda @4.9-4.26
				(args
					(p-ident @4.10-4.11 (raw "a")))
				(e-apply @4.13-4.26
					(e-tag @4.13-4.23 (raw "Maybe.Some"))
					(e-ident @4.24-4.25 (raw "a")))))
		(s-type-anno @6.1-6.18 (name "none1")
			(ty-apply @6.9-6.18
				(ty @6.9-6.14 (name "Maybe"))
				(underscore-ty-var @6.15-6.17 (raw "_a"))))
		(s-decl @7.1-7.19
			(p-ident @7.1-7.6 (raw "none1"))
			(e-tag @7.9-7.19 (raw "Maybe.None")))
		(s-decl @9.1-9.26
			(p-ident @9.1-9.6 (raw "some2"))
			(e-lambda @9.9-9.26
				(args
					(p-ident @9.10-9.11 (raw "a")))
				(e-apply @9.13-9.26
					(e-tag @9.13-9.23 (raw "Maybe.Some"))
					(e-ident @9.24-9.25 (raw "a")))))
		(s-decl @11.1-11.19
			(p-ident @11.1-11.6 (raw "none2"))
			(e-tag @11.9-11.19 (raw "Maybe.None")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.6 (ident "some1"))
		(e-lambda @4.9-4.26
			(args
				(p-assign @4.10-4.11 (ident "a")))
			(e-nominal @4.13-4.26 (nominal "Maybe")
				(e-tag @4.13-4.26 (name "Some")
					(args
						(e-lookup-local @4.24-4.25
							(p-assign @4.10-4.11 (ident "a")))))))
		(annotation @4.1-4.6
			(declared-type
				(ty-fn @3.9-3.22 (effectful false)
					(ty-var @3.9-3.10 (name "a"))
					(ty-apply @3.14-3.22 (symbol "Maybe")
						(ty-var @3.20-3.21 (name "a")))))))
	(d-let
		(p-assign @7.1-7.6 (ident "none1"))
		(e-nominal @7.9-7.19 (nominal "Maybe")
			(e-tag @7.9-7.19 (name "None")))
		(annotation @7.1-7.6
			(declared-type
				(ty-apply @6.9-6.18 (symbol "Maybe")
					(ty-var @6.15-6.17 (name "_a"))))))
	(d-let
		(p-assign @9.1-9.6 (ident "some2"))
		(e-lambda @9.9-9.26
			(args
				(p-assign @9.10-9.11 (ident "a")))
			(e-nominal @9.13-9.26 (nominal "Maybe")
				(e-tag @9.13-9.26 (name "Some")
					(args
						(e-lookup-local @9.24-9.25
							(p-assign @9.10-9.11 (ident "a"))))))))
	(d-let
		(p-assign @11.1-11.6 (ident "none2"))
		(e-nominal @11.9-11.19 (nominal "Maybe")
			(e-tag @11.9-11.19 (name "None"))))
	(s-nominal-decl @1.1-1.28
		(ty-header @1.1-1.9 (name "Maybe")
			(ty-args
				(ty-var @1.7-1.8 (name "a"))))
		(ty-tag-union @1.13-1.28
			(ty-apply @1.14-1.21 (symbol "Some")
				(ty-var @1.19-1.20 (name "a")))
			(ty @1.23-1.27 (name "None")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.6 (type "a -> Maybe(a)"))
		(patt @7.1-7.6 (type "Maybe(a)"))
		(patt @9.1-9.6 (type "a -> Maybe(a)"))
		(patt @11.1-11.6 (type "Maybe(a)")))
	(type_decls
		(nominal @1.1-1.28 (type "Maybe(a)")
			(ty-header @1.1-1.9 (name "Maybe")
				(ty-args
					(ty-var @1.7-1.8 (name "a"))))))
	(expressions
		(expr @4.9-4.26 (type "a -> Maybe(a)"))
		(expr @7.9-7.19 (type "Maybe(a)"))
		(expr @9.9-9.26 (type "a -> Maybe(a)"))
		(expr @11.9-11.19 (type "Maybe(a)"))))
~~~
