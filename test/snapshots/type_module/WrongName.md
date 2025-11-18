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

This file is named `WrongName`.roc, but no top-level type declaration named `WrongName` was found.

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
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "SomeOtherName")
				(args))
			(ty-tag-union
				(tags
					(ty (name "A"))
					(ty (name "B")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl
		(ty-header (name "SomeOtherName"))
		(ty-tag-union
			(ty-tag-name (name "A"))
			(ty-tag-name (name "B")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal (type "SomeOtherName")
			(ty-header (name "SomeOtherName"))))
	(expressions))
~~~
