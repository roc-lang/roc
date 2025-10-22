# META
~~~ini
description=Example of a simple nominal tag union
type=snippet
~~~
# SOURCE
~~~roc
Color := [Red, Green, Blue]

blue : Color
blue = Color.Blue

yellow : Color
yellow = Color.Yellow
~~~
# EXPECTED
INVALID NOMINAL TAG - nominal_tag_simple.md:7:10:7:22
# PROBLEMS
**INVALID NOMINAL TAG**
I'm having trouble with this nominal tag:
**nominal_tag_simple.md:7:10:7:22:**
```roc
yellow = Color.Yellow
```
         ^^^^^^^^^^^^

The tag is:
    _Yellow_

But the nominal type needs it to one of:
    _[Blue, Green, Red]_

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,
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
		(s-type-anno (name "blue")
			(ty (name "Color")))
		(s-decl
			(p-ident (raw "blue"))
			(e-tag (raw "Color.Blue")))
		(s-type-anno (name "yellow")
			(ty (name "Color")))
		(s-decl
			(p-ident (raw "yellow"))
			(e-tag (raw "Color.Yellow")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "blue"))
		(e-nominal (nominal "Color")
			(e-tag (name "Blue")))
		(annotation
			(ty-lookup (name "Color") (local))))
	(d-let
		(p-assign (ident "yellow"))
		(e-nominal (nominal "Color")
			(e-tag (name "Yellow")))
		(annotation
			(ty-lookup (name "Color") (local))))
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
		(patt (type "Error")))
	(type_decls
		(nominal (type "Color")
			(ty-header (name "Color"))))
	(expressions
		(expr (type "Color"))
		(expr (type "Error"))))
~~~
