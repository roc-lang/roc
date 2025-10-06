# META
~~~ini
description=Multiple unqualified type declarations
type=file:FileMultipleTypeDecls.roc
~~~
# SOURCE
~~~roc
FileMultipleTypeDecls := {}

FirstType : U64
SecondType : Str
ThirdType : List(U8)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:22),OpColonEqual(1:23-1:25),OpenCurly(1:26-1:27),CloseCurly(1:27-1:28),
UpperIdent(3:1-3:10),OpColon(3:11-3:12),UpperIdent(3:13-3:16),
UpperIdent(4:1-4:11),OpColon(4:12-4:13),UpperIdent(4:14-4:17),
UpperIdent(5:1-5:10),OpColon(5:11-5:12),UpperIdent(5:13-5:17),NoSpaceOpenRound(5:17-5:18),UpperIdent(5:18-5:20),CloseRound(5:20-5:21),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @1.1-5.21
	(type-module @1.1-1.22)
	(statements
		(s-type-decl @1.1-1.28
			(header @1.1-1.22 (name "FileMultipleTypeDecls")
				(args))
			(ty-record @1.26-1.28))
		(s-type-decl @3.1-3.16
			(header @3.1-3.10 (name "FirstType")
				(args))
			(ty @3.13-3.16 (name "U64")))
		(s-type-decl @4.1-4.17
			(header @4.1-4.11 (name "SecondType")
				(args))
			(ty @4.14-4.17 (name "Str")))
		(s-type-decl @5.1-5.21
			(header @5.1-5.10 (name "ThirdType")
				(args))
			(ty-apply @5.13-5.21
				(ty @5.13-5.17 (name "List"))
				(ty @5.18-5.20 (name "U8"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-1.28
		(ty-header @1.1-1.22 (name "FileMultipleTypeDecls"))
		(ty-record @1.26-1.28))
	(s-alias-decl @3.1-3.16
		(ty-header @3.1-3.10 (name "FirstType"))
		(ty-lookup @3.13-3.16 (name "U64") (builtin)))
	(s-alias-decl @4.1-4.17
		(ty-header @4.1-4.11 (name "SecondType"))
		(ty-lookup @4.14-4.17 (name "Str") (builtin)))
	(s-alias-decl @5.1-5.21
		(ty-header @5.1-5.10 (name "ThirdType"))
		(ty-apply @5.13-5.21 (name "List") (builtin)
			(ty-lookup @5.18-5.20 (name "U8") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.28 (type "FileMultipleTypeDecls")
			(ty-header @1.1-1.22 (name "FileMultipleTypeDecls")))
		(alias @3.1-3.16 (type "FirstType")
			(ty-header @3.1-3.10 (name "FirstType")))
		(alias @4.1-4.17 (type "SecondType")
			(ty-header @4.1-4.11 (name "SecondType")))
		(alias @5.1-5.21 (type "ThirdType")
			(ty-header @5.1-5.10 (name "ThirdType"))))
	(expressions))
~~~
