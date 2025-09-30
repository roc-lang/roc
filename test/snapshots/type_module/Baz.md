# META
~~~ini
description=Valid type module with multiple declarations, one matching
type=file
~~~
# SOURCE
~~~roc
Helper : I64

Baz := [X, Y, Z]

helper : Helper
helper = 42
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:7),OpColon(1:8-1:9),UpperIdent(1:10-1:13),
UpperIdent(3:1-3:4),OpColonEqual(3:5-3:7),OpenSquare(3:8-3:9),UpperIdent(3:9-3:10),Comma(3:10-3:11),UpperIdent(3:12-3:13),Comma(3:13-3:14),UpperIdent(3:15-3:16),CloseSquare(3:16-3:17),
LowerIdent(5:1-5:7),OpColon(5:8-5:9),UpperIdent(5:10-5:16),
LowerIdent(6:1-6:7),OpAssign(6:8-6:9),Int(6:10-6:12),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.12
	(type-module @1.1-1.7)
	(statements
		(s-type-decl @1.1-1.13
			(header @1.1-1.7 (name "Helper")
				(args))
			(ty @1.10-1.13 (name "I64")))
		(s-type-decl @3.1-3.17
			(header @3.1-3.4 (name "Baz")
				(args))
			(ty-tag-union @3.8-3.17
				(tags
					(ty @3.9-3.10 (name "X"))
					(ty @3.12-3.13 (name "Y"))
					(ty @3.15-3.16 (name "Z")))))
		(s-type-anno @5.1-5.16 (name "helper")
			(ty @5.10-5.16 (name "Helper")))
		(s-decl @6.1-6.12
			(p-ident @6.1-6.7 (raw "helper"))
			(e-int @6.10-6.12 (raw "42")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.7 (ident "helper"))
		(e-int @6.10-6.12 (value "42"))
		(annotation @6.1-6.7
			(declared-type
				(ty @5.10-5.16 (name "Helper")))))
	(s-alias-decl @1.1-1.13
		(ty-header @1.1-1.7 (name "Helper"))
		(ty @1.10-1.13 (name "I64")))
	(s-nominal-decl @3.1-3.17
		(ty-header @3.1-3.4 (name "Baz"))
		(ty-tag-union @3.8-3.17
			(ty @3.9-3.10 (name "X"))
			(ty @3.12-3.13 (name "Y"))
			(ty @3.15-3.16 (name "Z")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.7 (type "I64")))
	(type_decls
		(alias @1.1-1.13 (type "Helper")
			(ty-header @1.1-1.7 (name "Helper")))
		(nominal @3.1-3.17 (type "Baz")
			(ty-header @3.1-3.4 (name "Baz"))))
	(expressions
		(expr @6.10-6.12 (type "I64"))))
~~~
