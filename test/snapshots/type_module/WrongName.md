# META
~~~ini
description=Invalid type module - type name doesn't match module name
type=file
~~~
# SOURCE
~~~roc
SomeOtherName := [A, B]
~~~
# EXPECTED
TYPE MODULE MISSING MATCHING TYPE - WrongName.md:1:1:1:24
# PROBLEMS
**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This module is named `WrongName`, but no top-level type declaration named `WrongName` was found.

Add either:
`WrongName := ...` (nominal type)
or:
`WrongName : ...` (type alias)
**WrongName.md:1:1:1:24:**
```roc
SomeOtherName := [A, B]
```
^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:14),OpColonEqual(1:15-1:17),OpenSquare(1:18-1:19),UpperIdent(1:19-1:20),Comma(1:20-1:21),UpperIdent(1:22-1:23),CloseSquare(1:23-1:24),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.24
	(type-module @1.1-1.14)
	(statements
		(s-type-decl @1.1-1.24
			(header @1.1-1.14 (name "SomeOtherName")
				(args))
			(ty-tag-union @1.18-1.24
				(tags
					(ty @1.19-1.20 (name "A"))
					(ty @1.22-1.23 (name "B")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-1.24
		(ty-header @1.1-1.14 (name "SomeOtherName"))
		(ty-tag-union @1.18-1.24
			(ty @1.19-1.20 (name "A"))
			(ty @1.22-1.23 (name "B")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.24 (type "SomeOtherName")
			(ty-header @1.1-1.14 (name "SomeOtherName"))))
	(expressions))
~~~
