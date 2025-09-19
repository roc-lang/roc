# META
~~~ini
description=Example of mixed local and external nominal types in same scope
type=file
~~~
# SOURCE
~~~roc
module [LocalStatus, processColor]

LocalStatus := [Pending, Complete]

processColor : _ -> LocalStatus
processColor = |color| {

    # bring RGB into scope
    import Color.RGB

    match color {
        RGB.Red => LocalStatus.Pending
        RGB.Green => LocalStatus.Complete
        RGB.Blue => LocalStatus.Pending
    }
}
~~~
# EXPECTED
IMPORT MUST BE TOP LEVEL - nominal_mixed_scope.md:9:5:9:11
NOT IMPLEMENTED - :0:0:0:0
UNDECLARED TYPE - nominal_mixed_scope.md:9:12:9:17
UNDECLARED TYPE - nominal_mixed_scope.md:12:9:12:12
UNDECLARED TYPE - nominal_mixed_scope.md:13:9:13:12
UNDECLARED TYPE - nominal_mixed_scope.md:14:9:14:12
# PROBLEMS
**IMPORT MUST BE TOP LEVEL**
Import statements must appear at the top level of a module.
Move this import to the top of the file, after the module header but before any definitions.

**nominal_mixed_scope.md:9:5:9:11:**
```roc
    import Color.RGB
```
    ^^^^^^


**NOT IMPLEMENTED**
This feature is not yet implemented: statement type in block

This error doesn't have a proper diagnostic report yet. Let us know if you want to help improve Roc's error messages!

**UNDECLARED TYPE**
The type _Color_ is not declared in this scope.

This type is referenced here:
**nominal_mixed_scope.md:9:12:9:17:**
```roc
    import Color.RGB
```
           ^^^^^


**UNDECLARED TYPE**
The type _RGB_ is not declared in this scope.

This type is referenced here:
**nominal_mixed_scope.md:12:9:12:12:**
```roc
        RGB.Red => LocalStatus.Pending
```
        ^^^


**UNDECLARED TYPE**
The type _RGB_ is not declared in this scope.

This type is referenced here:
**nominal_mixed_scope.md:13:9:13:12:**
```roc
        RGB.Green => LocalStatus.Complete
```
        ^^^


**UNDECLARED TYPE**
The type _RGB_ is not declared in this scope.

This type is referenced here:
**nominal_mixed_scope.md:14:9:14:12:**
```roc
        RGB.Blue => LocalStatus.Pending
```
        ^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:20),Comma(1:20-1:21),LowerIdent(1:22-1:34),CloseSquare(1:34-1:35),
UpperIdent(3:1-3:12),OpColonEqual(3:13-3:15),OpenSquare(3:16-3:17),UpperIdent(3:17-3:24),Comma(3:24-3:25),UpperIdent(3:26-3:34),CloseSquare(3:34-3:35),
LowerIdent(5:1-5:13),OpColon(5:14-5:15),Underscore(5:16-5:17),OpArrow(5:18-5:20),UpperIdent(5:21-5:32),
LowerIdent(6:1-6:13),OpAssign(6:14-6:15),OpBar(6:16-6:17),LowerIdent(6:17-6:22),OpBar(6:22-6:23),OpenCurly(6:24-6:25),
KwImport(9:5-9:11),UpperIdent(9:12-9:17),NoSpaceDotUpperIdent(9:17-9:21),
KwMatch(11:5-11:10),LowerIdent(11:11-11:16),OpenCurly(11:17-11:18),
UpperIdent(12:9-12:12),NoSpaceDotUpperIdent(12:12-12:16),OpFatArrow(12:17-12:19),UpperIdent(12:20-12:31),NoSpaceDotUpperIdent(12:31-12:39),
UpperIdent(13:9-13:12),NoSpaceDotUpperIdent(13:12-13:18),OpFatArrow(13:19-13:21),UpperIdent(13:22-13:33),NoSpaceDotUpperIdent(13:33-13:42),
UpperIdent(14:9-14:12),NoSpaceDotUpperIdent(14:12-14:17),OpFatArrow(14:18-14:20),UpperIdent(14:21-14:32),NoSpaceDotUpperIdent(14:32-14:40),
CloseCurly(15:5-15:6),
CloseCurly(16:1-16:2),
EndOfFile(17:1-17:1),
~~~
# PARSE
~~~clojure
(file @1.1-16.2
	(module @1.1-1.35
		(exposes @1.8-1.35
			(exposed-upper-ident @1.9-1.20 (text "LocalStatus"))
			(exposed-lower-ident @1.22-1.34
				(text "processColor"))))
	(statements
		(s-type-decl @3.1-3.35
			(header @3.1-3.12 (name "LocalStatus")
				(args))
			(ty-tag-union @3.16-3.35
				(tags
					(ty @3.17-3.24 (name "Pending"))
					(ty @3.26-3.34 (name "Complete")))))
		(s-type-anno @5.1-5.32 (name "processColor")
			(ty-fn @5.16-5.32
				(_)
				(ty @5.21-5.32 (name "LocalStatus"))))
		(s-decl @6.1-16.2
			(p-ident @6.1-6.13 (raw "processColor"))
			(e-lambda @6.16-16.2
				(args
					(p-ident @6.17-6.22 (raw "color")))
				(e-block @6.24-16.2
					(statements
						(s-malformed @9.5-9.11 (tag "import_must_be_top_level"))
						(e-tag @9.12-9.21 (raw "Color.RGB"))
						(e-match
							(e-ident @11.11-11.16 (raw "color"))
							(branches
								(branch @12.9-12.39
									(p-tag @12.9-12.16 (raw ".Red"))
									(e-tag @12.20-12.39 (raw "LocalStatus.Pending")))
								(branch @13.9-13.42
									(p-tag @13.9-13.18 (raw ".Green"))
									(e-tag @13.22-13.42 (raw "LocalStatus.Complete")))
								(branch @14.9-14.40
									(p-tag @14.9-14.17 (raw ".Blue"))
									(e-tag @14.21-14.40 (raw "LocalStatus.Pending")))))))))))
~~~
# FORMATTED
~~~roc
module [LocalStatus, processColor]

LocalStatus := [Pending, Complete]

processColor : _ -> LocalStatus
processColor = |color| {

	# bring RGB into scope
		Color.RGB

	match color {
		RGB.Red => LocalStatus.Pending
		RGB.Green => LocalStatus.Complete
		RGB.Blue => LocalStatus.Pending
	}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.13 (ident "processColor"))
		(e-lambda @6.16-16.2
			(args
				(p-assign @6.17-6.22 (ident "color")))
			(e-block @6.24-16.2
				(s-runtime-error (tag "not_implemented"))
				(s-expr @9.12-9.21
					(e-runtime-error (tag "undeclared_type")))
				(e-match @11.5-15.6
					(match @11.5-15.6
						(cond
							(e-lookup-local @11.11-11.16
								(p-assign @6.17-6.22 (ident "color"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-runtime-error @12.9-12.12 (tag "undeclared_type"))))
								(value
									(e-nominal @12.20-12.39 (nominal "LocalStatus")
										(e-tag @12.20-12.39 (name "Pending")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-runtime-error @13.9-13.12 (tag "undeclared_type"))))
								(value
									(e-nominal @13.22-13.42 (nominal "LocalStatus")
										(e-tag @13.22-13.42 (name "Complete")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-runtime-error @14.9-14.12 (tag "undeclared_type"))))
								(value
									(e-nominal @14.21-14.40 (nominal "LocalStatus")
										(e-tag @14.21-14.40 (name "Pending"))))))))))
		(annotation @6.1-6.13
			(declared-type
				(ty-fn @5.16-5.32 (effectful false)
					(ty-underscore @1.1-1.1)
					(ty-lookup @5.21-5.32 (name "LocalStatus") (local))))))
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Bool"))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "True"))
			(tag_name @1.1-1.1 (name "False"))))
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Result")
			(ty-args
				(ty-rigid-var @1.1-1.1 (name "ok"))
				(ty-rigid-var @1.1-1.1 (name "err"))))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "Ok"))
			(tag_name @1.1-1.1 (name "Err"))))
	(s-nominal-decl @3.1-3.35
		(ty-header @3.1-3.12 (name "LocalStatus"))
		(ty-tag-union @3.16-3.35
			(tag_name @3.17-3.24 (name "Pending"))
			(tag_name @3.26-3.34 (name "Complete")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.13 (type "_arg -> LocalStatus")))
	(type_decls
		(nominal @1.1-1.1 (type "Bool")
			(ty-header @1.1-1.1 (name "Bool")))
		(nominal @1.1-1.1 (type "Result(ok, err)")
			(ty-header @1.1-1.1 (name "Result")
				(ty-args
					(ty-rigid-var @1.1-1.1 (name "ok"))
					(ty-rigid-var @1.1-1.1 (name "err")))))
		(nominal @3.1-3.35 (type "LocalStatus")
			(ty-header @3.1-3.12 (name "LocalStatus"))))
	(expressions
		(expr @6.16-16.2 (type "_arg -> LocalStatus"))))
~~~
