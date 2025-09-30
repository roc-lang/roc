# META
~~~ini
description=Multiple unqualified type declarations
type=file
~~~
# SOURCE
~~~roc
FirstType : U64
SecondType : Str
ThirdType : List(U8)
~~~
# EXPECTED
TYPE MODULE MISSING MATCHING TYPE - file_multiple_type_decls.md:1:1:3:21
# PROBLEMS
**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `file_multiple_type_decls.roc`, but no top-level type declaration named `file_multiple_type_decls` was found.

Add either:
`file_multiple_type_decls := ...` (nominal type)
or:
`file_multiple_type_decls : ...` (type alias)
**file_multiple_type_decls.md:1:1:3:21:**
```roc
FirstType : U64
SecondType : Str
ThirdType : List(U8)
```


# TOKENS
~~~zig
UpperIdent(1:1-1:10),OpColon(1:11-1:12),UpperIdent(1:13-1:16),
UpperIdent(2:1-2:11),OpColon(2:12-2:13),UpperIdent(2:14-2:17),
UpperIdent(3:1-3:10),OpColon(3:11-3:12),UpperIdent(3:13-3:17),NoSpaceOpenRound(3:17-3:18),UpperIdent(3:18-3:20),CloseRound(3:20-3:21),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.21
	(type-module @1.1-1.10)
	(statements
		(s-type-decl @1.1-1.16
			(header @1.1-1.10 (name "FirstType")
				(args))
			(ty @1.13-1.16 (name "U64")))
		(s-type-decl @2.1-2.17
			(header @2.1-2.11 (name "SecondType")
				(args))
			(ty @2.14-2.17 (name "Str")))
		(s-type-decl @3.1-3.21
			(header @3.1-3.10 (name "ThirdType")
				(args))
			(ty-apply @3.13-3.21
				(ty @3.13-3.17 (name "List"))
				(ty @3.18-3.20 (name "U8"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @1.1-1.16
		(ty-header @1.1-1.10 (name "FirstType"))
		(ty @1.13-1.16 (name "U64")))
	(s-alias-decl @2.1-2.17
		(ty-header @2.1-2.11 (name "SecondType"))
		(ty @2.14-2.17 (name "Str")))
	(s-alias-decl @3.1-3.21
		(ty-header @3.1-3.10 (name "ThirdType"))
		(ty-apply @3.13-3.21 (symbol "List")
			(ty @3.18-3.20 (name "U8")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @1.1-1.16 (type "FirstType")
			(ty-header @1.1-1.10 (name "FirstType")))
		(alias @2.1-2.17 (type "SecondType")
			(ty-header @2.1-2.11 (name "SecondType")))
		(alias @3.1-3.21 (type "ThirdType")
			(ty-header @3.1-3.10 (name "ThirdType"))))
	(expressions))
~~~
