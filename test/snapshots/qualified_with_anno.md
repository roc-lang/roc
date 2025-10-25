# META
~~~ini
description=Test qualified tag with type annotation
type=snippet
~~~
# SOURCE
~~~roc
MyType := [TagA, TagB]

value : MyType
value = MyType.TagA
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
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
			(header (name "MyType")
				(args))
			(ty-tag-union
				(tags
					(ty (name "TagA"))
					(ty (name "TagB")))))
		(s-type-anno (name "value")
			(ty (name "MyType")))
		(s-decl
			(p-ident (raw "value"))
			(e-tag (raw "MyType.TagA")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "value"))
		(e-nominal (nominal "MyType")
			(e-tag (name "TagA")))
		(annotation
			(ty-lookup (name "MyType") (local))))
	(s-nominal-decl
		(ty-header (name "MyType"))
		(ty-tag-union
			(ty-tag-name (name "TagA"))
			(ty-tag-name (name "TagB")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "MyType")))
	(type_decls
		(nominal (type "MyType")
			(ty-header (name "MyType"))))
	(expressions
		(expr (type "MyType"))))
~~~
