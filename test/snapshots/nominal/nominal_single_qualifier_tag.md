# META
~~~ini
description=Tag with single qualifier
type=snippet
~~~
# SOURCE
~~~roc
Foo := [X, Y, Z]

x = Foo.X
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty-tag-union
				(tags
					(ty (name "X"))
					(ty (name "Y"))
					(ty (name "Z")))))
		(s-decl
			(p-ident (raw "x"))
			(e-tag (raw "Foo.X")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-nominal (nominal "Foo")
			(e-tag (name "X"))))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "X"))
			(ty-tag-name (name "Y"))
			(ty-tag-name (name "Z")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Foo")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo"))))
	(expressions
		(expr (type "Foo"))))
~~~
