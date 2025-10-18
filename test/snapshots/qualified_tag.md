# META
~~~ini
description=Simple qualified tag test
type=snippet
~~~
# SOURCE
~~~roc
Color := [Red, Blue]

test = Color.Red
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
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
					(ty (name "Blue")))))
		(s-decl
			(p-ident (raw "test"))
			(e-tag (raw "Color.Red")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "test"))
		(e-nominal (nominal "Color")
			(e-tag (name "Red"))))
	(s-nominal-decl
		(ty-header (name "Color"))
		(ty-tag-union
			(ty-tag-name (name "Red"))
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
