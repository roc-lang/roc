# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
Foo(a,b) : (a,b,Str,U64)
~~~
# EXPECTED
TYPE MODULE MISSING MATCHING TYPE - stmt_type_decl.md:1:1:1:25
# PROBLEMS
**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `stmt_type_decl.roc`, but no top-level type declaration named `stmt_type_decl` was found.

Add either:
`stmt_type_decl := ...` (nominal type)
or:
`stmt_type_decl : ...` (type alias)
**stmt_type_decl.md:1:1:1:25:**
```roc
Foo(a,b) : (a,b,Str,U64)
```
^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:4),NoSpaceOpenRound(1:4-1:5),LowerIdent(1:5-1:6),Comma(1:6-1:7),LowerIdent(1:7-1:8),CloseRound(1:8-1:9),OpColon(1:10-1:11),OpenRound(1:12-1:13),LowerIdent(1:13-1:14),Comma(1:14-1:15),LowerIdent(1:15-1:16),Comma(1:16-1:17),UpperIdent(1:17-1:20),Comma(1:20-1:21),UpperIdent(1:21-1:24),CloseRound(1:24-1:25),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.25
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-1.25
			(header @1.1-1.9 (name "Foo")
				(args
					(ty-var @1.5-1.6 (raw "a"))
					(ty-var @1.7-1.8 (raw "b"))))
			(ty-tuple @1.12-1.25
				(ty-var @1.13-1.14 (raw "a"))
				(ty-var @1.15-1.16 (raw "b"))
				(ty @1.17-1.20 (name "Str"))
				(ty @1.21-1.24 (name "U64"))))))
~~~
# FORMATTED
~~~roc
Foo(a, b) : (a, b, Str, U64)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl @1.1-1.25
		(ty-header @1.1-1.9 (name "Foo")
			(ty-args
				(ty-var @1.5-1.6 (name "a"))
				(ty-var @1.7-1.8 (name "b"))))
		(ty-tuple @1.12-1.25
			(ty-var @1.13-1.14 (name "a"))
			(ty-var @1.15-1.16 (name "b"))
			(ty @1.17-1.20 (name "Str"))
			(ty @1.21-1.24 (name "U64")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias @1.1-1.25 (type "Foo(a, b)")
			(ty-header @1.1-1.9 (name "Foo")
				(ty-args
					(ty-var @1.5-1.6 (name "a"))
					(ty-var @1.7-1.8 (name "b"))))))
	(expressions))
~~~
