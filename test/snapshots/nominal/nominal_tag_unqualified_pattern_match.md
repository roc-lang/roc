# META
~~~ini
description=Pattern match on nominal tag union with unqualified tags
type=snippet
~~~
# SOURCE
~~~roc
Color := [Red, Green, Blue]

isRed : Color -> Bool
isRed = |color| match color {
    Red => Bool.True
    Green => Bool.False
    Blue => Bool.False
}
~~~
# EXPECTED
UNDECLARED TYPE - nominal_tag_unqualified_pattern_match.md:3:18:3:22
UNDECLARED TYPE - nominal_tag_unqualified_pattern_match.md:5:12:5:16
UNDECLARED TYPE - nominal_tag_unqualified_pattern_match.md:6:14:6:18
UNDECLARED TYPE - nominal_tag_unqualified_pattern_match.md:7:13:7:17
# PROBLEMS
**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**nominal_tag_unqualified_pattern_match.md:3:18:3:22:**
```roc
isRed : Color -> Bool
```
                 ^^^^


**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**nominal_tag_unqualified_pattern_match.md:5:12:5:16:**
```roc
    Red => Bool.True
```
           ^^^^


**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**nominal_tag_unqualified_pattern_match.md:6:14:6:18:**
```roc
    Green => Bool.False
```
             ^^^^


**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**nominal_tag_unqualified_pattern_match.md:7:13:7:17:**
```roc
    Blue => Bool.False
```
            ^^^^


# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
UpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
UpperIdent,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
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
							(p-tag (raw "Red"))
							(e-tag (raw "Bool.True")))
						(branch
							(p-tag (raw "Green"))
							(e-tag (raw "Bool.False")))
						(branch
							(p-tag (raw "Blue"))
							(e-tag (raw "Bool.False")))))))))
~~~
# FORMATTED
~~~roc
Color := [Red, Green, Blue]

isRed : Color -> Bool
isRed = |color| match color {
	Red => Bool.True
	Green => Bool.False
	Blue => Bool.False
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
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
									(p-applied-tag)))
							(value
								(e-runtime-error (tag "undeclared_type"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-runtime-error (tag "undeclared_type"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
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
		(patt (type "Color -> Error")))
	(type_decls
		(nominal (type "Color")
			(ty-header (name "Color"))))
	(expressions
		(expr (type "Color -> Error"))))
~~~
