# META
~~~ini
description=Qualified syntax still works (backward compatibility)
type=snippet
~~~
# SOURCE
~~~roc
Color := [Red, Green, Blue]

myColor : Color
myColor = Color.Red

isRed : Color -> Bool
isRed = |color| match color {
    Color.Red => Bool.True
    Color.Green => Bool.False
    Color.Blue => Bool.False
}
~~~
# EXPECTED
UNDECLARED TYPE - nominal_tag_qualified_still_works.md:6:18:6:22
UNDECLARED TYPE - nominal_tag_qualified_still_works.md:8:18:8:22
UNDECLARED TYPE - nominal_tag_qualified_still_works.md:9:20:9:24
UNDECLARED TYPE - nominal_tag_qualified_still_works.md:10:19:10:23
# PROBLEMS
**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**nominal_tag_qualified_still_works.md:6:18:6:22:**
```roc
isRed : Color -> Bool
```
                 ^^^^


**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**nominal_tag_qualified_still_works.md:8:18:8:22:**
```roc
    Color.Red => Bool.True
```
                 ^^^^


**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**nominal_tag_qualified_still_works.md:9:20:9:24:**
```roc
    Color.Green => Bool.False
```
                   ^^^^


**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**nominal_tag_qualified_still_works.md:10:19:10:23:**
```roc
    Color.Blue => Bool.False
```
                  ^^^^


# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceDotUpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
UpperIdent,NoSpaceDotUpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
UpperIdent,NoSpaceDotUpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Color")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Red"))
					(ty (name "Green"))
					(ty (name "Blue")))))
		(s-type-anno (name "myColor")
			(ty (name "Color")))
		(s-decl
			(p-ident (raw "myColor"))
			(e-tag (raw "Color.Red")))
		(s-type-anno (name "isRed")
			(ty-fn
				(ty (name "Color"))
				(ty (name "Bool"))))
		(s-decl
			(p-ident (raw "isRed"))
			(e-lambda
				(args
					(p-ident (raw "color")))
				(e-match
					(e-ident (raw "color"))
					(branches
						(branch
							(p-tag (raw ".Red"))
							(e-tag (raw "Bool.True")))
						(branch
							(p-tag (raw ".Green"))
							(e-tag (raw "Bool.False")))
						(branch
							(p-tag (raw ".Blue"))
							(e-tag (raw "Bool.False")))))))))
~~~
# FORMATTED
~~~roc
Color := [Red, Green, Blue]

myColor : Color
myColor = Color.Red

isRed : Color -> Bool
isRed = |color| match color {
	Color.Red => Bool.True
	Color.Green => Bool.False
	Color.Blue => Bool.False
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "myColor"))
		(e-nominal (nominal "Color")
			(e-tag (name "Red")))
		(annotation
			(declared-type
				(ty-lookup (name "Color") (local)))))
	(d-let
		(p-assign (ident "isRed"))
		(e-lambda
			(args
				(p-assign (ident "color")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "color"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal
										(p-applied-tag))))
							(value
								(e-runtime-error (tag "undeclared_type"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal
										(p-applied-tag))))
							(value
								(e-runtime-error (tag "undeclared_type"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal
										(p-applied-tag))))
							(value
								(e-runtime-error (tag "undeclared_type"))))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "Color") (local))
					(ty-malformed)))))
	(s-nominal-decl
		(ty-header (name "Color"))
		(ty-tag-union
			(ty-tag-name (name "Red"))
			(ty-tag-name (name "Green"))
			(ty-tag-name (name "Blue")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Color"))
		(patt (type "Color -> Error")))
	(type_decls
		(nominal (type "Color")
			(ty-header (name "Color"))))
	(expressions
		(expr (type "Color"))
		(expr (type "Color -> Error"))))
~~~
