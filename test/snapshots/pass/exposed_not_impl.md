# META
~~~ini
description=Module exposes values that are not implemented
type=file
~~~
# SOURCE
~~~roc
# This module exposes foo, bar, MyType, and OtherType
# but only implements foo and MyType
# This should generate "exposed but not implemented" errors for bar and OtherType
# Also tests redundant exposed entries for foo and MyType

foo = 42

MyType : [A, B, C]
~~~
# EXPECTED
TYPE MODULE MISSING MATCHING TYPE - exposed_not_impl.md:6:1:8:19
# PROBLEMS
**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `exposed_not_impl.roc`, but no top-level type declaration named `exposed_not_impl` was found.

Add either:
`exposed_not_impl := ...` (nominal type)
or:
`exposed_not_impl : ...` (type alias)
**exposed_not_impl.md:6:1:8:19:**
```roc
foo = 42

MyType : [A, B, C]
```


# TOKENS
~~~zig
LowerIdent(6:1-6:4),OpAssign(6:5-6:6),Int(6:7-6:9),
UpperIdent(8:1-8:7),OpColon(8:8-8:9),OpenSquare(8:10-8:11),UpperIdent(8:11-8:12),Comma(8:12-8:13),UpperIdent(8:14-8:15),Comma(8:15-8:16),UpperIdent(8:17-8:18),CloseSquare(8:18-8:19),
EndOfFile(9:1-9:1),
~~~
# PARSE
~~~clojure
(file @6.1-8.19
	(type-module @6.1-6.4)
	(statements
		(s-decl @6.1-6.9
			(p-ident @6.1-6.4 (raw "foo"))
			(e-int @6.7-6.9 (raw "42")))
		(s-type-decl @8.1-8.19
			(header @8.1-8.7 (name "MyType")
				(args))
			(ty-tag-union @8.10-8.19
				(tags
					(ty @8.11-8.12 (name "A"))
					(ty @8.14-8.15 (name "B"))
					(ty @8.17-8.18 (name "C")))))))
~~~
# FORMATTED
~~~roc
# This module exposes foo, bar, MyType, and OtherType
# but only implements foo and MyType
# This should generate "exposed but not implemented" errors for bar and OtherType
# Also tests redundant exposed entries for foo and MyType

# This module exposes foo, bar, MyType, and OtherType
# but only implements foo and MyType
# This should generate "exposed but not implemented" errors for bar and OtherType
# Also tests redundant exposed entries for foo and MyType

foo = 42

MyType : [A, B, C]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.4 (ident "foo"))
		(e-int @6.7-6.9 (value "42")))
	(s-alias-decl @8.1-8.19
		(ty-header @8.1-8.7 (name "MyType"))
		(ty-tag-union @8.10-8.19
			(ty @8.11-8.12 (name "A"))
			(ty @8.14-8.15 (name "B"))
			(ty @8.17-8.18 (name "C")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.4 (type "Num(_size)")))
	(type_decls
		(alias @8.1-8.19 (type "MyType")
			(ty-header @8.1-8.7 (name "MyType"))))
	(expressions
		(expr @6.7-6.9 (type "Num(_size)"))))
~~~
