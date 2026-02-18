# META
~~~ini
description=Construct nominal tag union value with unqualified tag
type=snippet
~~~
# SOURCE
~~~roc
Color := [Red, Green, Blue]

myColor : Color
myColor = Red
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,
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
			(e-tag (raw "Red")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "myColor"))
		(e-tag (name "Red"))
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
		(patt (type "Color")))
	(type_decls
		(nominal (type "Color")
			(ty-header (name "Color"))))
	(expressions
		(expr (type "Color"))))
~~~
