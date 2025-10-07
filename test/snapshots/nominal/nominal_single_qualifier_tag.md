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
UpperIdent(1:1-1:4),OpColonEqual(1:5-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:10),Comma(1:10-1:11),UpperIdent(1:12-1:13),Comma(1:13-1:14),UpperIdent(1:15-1:16),CloseSquare(1:16-1:17),
LowerIdent(3:1-3:2),OpAssign(3:3-3:4),UpperIdent(3:5-3:8),NoSpaceDotUpperIdent(3:8-3:10),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.10
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-1.17
			(header @1.1-1.4 (name "Foo")
				(args))
			(ty-tag-union @1.8-1.17
				(tags
					(ty @1.9-1.10 (name "X"))
					(ty @1.12-1.13 (name "Y"))
					(ty @1.15-1.16 (name "Z")))))
		(s-decl @3.1-3.10
			(p-ident @3.1-3.2 (raw "x"))
			(e-tag @3.5-3.10 (raw "Foo.X")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.2 (ident "x"))
		(e-nominal @3.5-3.10 (nominal "Foo")
			(e-tag @3.5-3.10 (name "X"))))
	(s-nominal-decl @1.1-1.17
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.17
			(ty-tag-name @1.9-1.10 (name "X"))
			(ty-tag-name @1.12-1.13 (name "Y"))
			(ty-tag-name @1.15-1.16 (name "Z")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.2 (type "Foo")))
	(type_decls
		(nominal @1.1-1.17 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo"))))
	(expressions
		(expr @3.5-3.10 (type "Foo"))))
~~~
