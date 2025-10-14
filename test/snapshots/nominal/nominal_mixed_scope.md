# META
~~~ini
description=Example of mixed local and external nominal types in same scope
type=snippet
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
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpColon,Underscore,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwImport,UpperIdent,NoSpaceDotUpperIdent,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceDotUpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
UpperIdent,NoSpaceDotUpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
UpperIdent,NoSpaceDotUpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "LocalStatus")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Pending"))
					(ty (name "Complete")))))
		(s-type-anno (name "processColor")
			(ty-fn
				(_)
				(ty (name "LocalStatus"))))
		(s-decl
			(p-ident (raw "processColor"))
			(e-lambda
				(args
					(p-ident (raw "color")))
				(e-block
					(statements
						(s-malformed (tag "import_must_be_top_level"))
						(e-tag (raw "Color.RGB"))
						(e-match
							(e-ident (raw "color"))
							(branches
								(branch
									(p-tag (raw ".Red"))
									(e-tag (raw "LocalStatus.Pending")))
								(branch
									(p-tag (raw ".Green"))
									(e-tag (raw "LocalStatus.Complete")))
								(branch
									(p-tag (raw ".Blue"))
									(e-tag (raw "LocalStatus.Pending")))))))))))
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
		(p-assign (ident "processColor"))
		(e-lambda
			(args
				(p-assign (ident "color")))
			(e-block
				(s-runtime-error (tag "not_implemented"))
				(s-expr
					(e-runtime-error (tag "undeclared_type")))
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "color"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-runtime-error (tag "undeclared_type"))))
								(value
									(e-nominal (nominal "LocalStatus")
										(e-tag (name "Pending")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-runtime-error (tag "undeclared_type"))))
								(value
									(e-nominal (nominal "LocalStatus")
										(e-tag (name "Complete")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-runtime-error (tag "undeclared_type"))))
								(value
									(e-nominal (nominal "LocalStatus")
										(e-tag (name "Pending"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-underscore)
				(ty-lookup (name "LocalStatus") (local)))))
	(s-nominal-decl
		(ty-header (name "LocalStatus"))
		(ty-tag-union
			(ty-tag-name (name "Pending"))
			(ty-tag-name (name "Complete")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> LocalStatus")))
	(type_decls
		(nominal (type "LocalStatus")
			(ty-header (name "LocalStatus"))))
	(expressions
		(expr (type "_arg -> LocalStatus"))))
~~~
