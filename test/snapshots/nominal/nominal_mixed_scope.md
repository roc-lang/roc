# META
~~~ini
description=Example of mixed local and external nominal types in same scope
type=file:LocalStatus.roc
~~~
# SOURCE
~~~roc
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
IMPORT MUST BE TOP LEVEL - nominal_mixed_scope.md:7:5:7:11
NOT IMPLEMENTED - :0:0:0:0
UNDECLARED TYPE - nominal_mixed_scope.md:7:12:7:17
UNDECLARED TYPE - nominal_mixed_scope.md:10:9:10:12
UNDECLARED TYPE - nominal_mixed_scope.md:11:9:11:12
UNDECLARED TYPE - nominal_mixed_scope.md:12:9:12:12
# PROBLEMS
**IMPORT MUST BE TOP LEVEL**
Import statements must appear at the top level of a module.
Move this import to the top of the file, after the module header but before any definitions.

**nominal_mixed_scope.md:7:5:7:11:**
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
**nominal_mixed_scope.md:7:12:7:17:**
```roc
    import Color.RGB
```
           ^^^^^


**UNDECLARED TYPE**
The type _RGB_ is not declared in this scope.

This type is referenced here:
**nominal_mixed_scope.md:10:9:10:12:**
```roc
        RGB.Red => LocalStatus.Pending
```
        ^^^


**UNDECLARED TYPE**
The type _RGB_ is not declared in this scope.

This type is referenced here:
**nominal_mixed_scope.md:11:9:11:12:**
```roc
        RGB.Green => LocalStatus.Complete
```
        ^^^


**UNDECLARED TYPE**
The type _RGB_ is not declared in this scope.

This type is referenced here:
**nominal_mixed_scope.md:12:9:12:12:**
```roc
        RGB.Blue => LocalStatus.Pending
```
        ^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:12),OpColonEqual(1:13-1:15),OpenSquare(1:16-1:17),UpperIdent(1:17-1:24),Comma(1:24-1:25),UpperIdent(1:26-1:34),CloseSquare(1:34-1:35),
LowerIdent(3:1-3:13),OpColon(3:14-3:15),Underscore(3:16-3:17),OpArrow(3:18-3:20),UpperIdent(3:21-3:32),
LowerIdent(4:1-4:13),OpAssign(4:14-4:15),OpBar(4:16-4:17),LowerIdent(4:17-4:22),OpBar(4:22-4:23),OpenCurly(4:24-4:25),
KwImport(7:5-7:11),UpperIdent(7:12-7:17),NoSpaceDotUpperIdent(7:17-7:21),
KwMatch(9:5-9:10),LowerIdent(9:11-9:16),OpenCurly(9:17-9:18),
UpperIdent(10:9-10:12),NoSpaceDotUpperIdent(10:12-10:16),OpFatArrow(10:17-10:19),UpperIdent(10:20-10:31),NoSpaceDotUpperIdent(10:31-10:39),
UpperIdent(11:9-11:12),NoSpaceDotUpperIdent(11:12-11:18),OpFatArrow(11:19-11:21),UpperIdent(11:22-11:33),NoSpaceDotUpperIdent(11:33-11:42),
UpperIdent(12:9-12:12),NoSpaceDotUpperIdent(12:12-12:17),OpFatArrow(12:18-12:20),UpperIdent(12:21-12:32),NoSpaceDotUpperIdent(12:32-12:40),
CloseCurly(13:5-13:6),
CloseCurly(14:1-14:2),
EndOfFile(15:1-15:1),
~~~
# PARSE
~~~clojure
(file @1.1-14.2
	(type-module @1.1-1.12)
	(statements
		(s-type-decl @1.1-1.35
			(header @1.1-1.12 (name "LocalStatus")
				(args))
			(ty-tag-union @1.16-1.35
				(tags
					(ty @1.17-1.24 (name "Pending"))
					(ty @1.26-1.34 (name "Complete")))))
		(s-type-anno @3.1-3.32 (name "processColor")
			(ty-fn @3.16-3.32
				(_)
				(ty @3.21-3.32 (name "LocalStatus"))))
		(s-decl @4.1-14.2
			(p-ident @4.1-4.13 (raw "processColor"))
			(e-lambda @4.16-14.2
				(args
					(p-ident @4.17-4.22 (raw "color")))
				(e-block @4.24-14.2
					(statements
						(s-malformed @7.5-7.11 (tag "import_must_be_top_level"))
						(e-tag @7.12-7.21 (raw "Color.RGB"))
						(e-match
							(e-ident @9.11-9.16 (raw "color"))
							(branches
								(branch @10.9-10.39
									(p-tag @10.9-10.16 (raw ".Red"))
									(e-tag @10.20-10.39 (raw "LocalStatus.Pending")))
								(branch @11.9-11.42
									(p-tag @11.9-11.18 (raw ".Green"))
									(e-tag @11.22-11.42 (raw "LocalStatus.Complete")))
								(branch @12.9-12.40
									(p-tag @12.9-12.17 (raw ".Blue"))
									(e-tag @12.21-12.40 (raw "LocalStatus.Pending")))))))))))
~~~
# FORMATTED
~~~roc
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
		(p-assign @4.1-4.13 (ident "processColor"))
		(e-lambda @4.16-14.2
			(args
				(p-assign @4.17-4.22 (ident "color")))
			(e-block @4.24-14.2
				(s-runtime-error (tag "not_implemented"))
				(s-expr @7.12-7.21
					(e-runtime-error (tag "undeclared_type")))
				(e-match @9.5-13.6
					(match @9.5-13.6
						(cond
							(e-lookup-local @9.11-9.16
								(p-assign @4.17-4.22 (ident "color"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-runtime-error @10.9-10.12 (tag "undeclared_type"))))
								(value
									(e-nominal @10.20-10.39 (nominal "LocalStatus")
										(e-tag @10.20-10.39 (name "Pending")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-runtime-error @11.9-11.12 (tag "undeclared_type"))))
								(value
									(e-nominal @11.22-11.42 (nominal "LocalStatus")
										(e-tag @11.22-11.42 (name "Complete")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-runtime-error @12.9-12.12 (tag "undeclared_type"))))
								(value
									(e-nominal @12.21-12.40 (nominal "LocalStatus")
										(e-tag @12.21-12.40 (name "Pending"))))))))))
		(annotation @4.1-4.13
			(declared-type
				(ty-fn @3.16-3.32 (effectful false)
					(ty-underscore @1.1-1.1)
					(ty @3.21-3.32 (name "LocalStatus"))))))
	(s-nominal-decl @1.1-1.35
		(ty-header @1.1-1.12 (name "LocalStatus"))
		(ty-tag-union @1.16-1.35
			(ty @1.17-1.24 (name "Pending"))
			(ty @1.26-1.34 (name "Complete")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.13 (type "Error -> LocalStatus")))
	(type_decls
		(nominal @1.1-1.35 (type "LocalStatus")
			(ty-header @1.1-1.12 (name "LocalStatus"))))
	(expressions
		(expr @4.16-14.2 (type "Error -> LocalStatus"))))
~~~
