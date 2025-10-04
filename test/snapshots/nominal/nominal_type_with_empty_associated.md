# META
~~~ini
description=Nominal type with empty associated items block
type=file
~~~
# SOURCE
~~~roc
module []

Foo := [A, B, C].{}
~~~
# EXPECTED
MODULE HEADER DEPRECATED - nominal_type_with_empty_associated.md:1:1:1:10
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**nominal_type_with_empty_associated.md:1:1:1:10:**
```roc
module []
```
^^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
UpperIdent(3:1-3:4),OpColonEqual(3:5-3:7),OpenSquare(3:8-3:9),UpperIdent(3:9-3:10),Comma(3:10-3:11),UpperIdent(3:12-3:13),Comma(3:13-3:14),UpperIdent(3:15-3:16),CloseSquare(3:16-3:17),Dot(3:17-3:18),OpenCurly(3:18-3:19),CloseCurly(3:19-3:20),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.20
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-decl @3.1-3.20
			(header @3.1-3.4 (name "Foo")
				(args))
			(ty-tag-union @3.8-3.17
				(tags
					(ty @3.9-3.10 (name "A"))
					(ty @3.12-3.13 (name "B"))
					(ty @3.15-3.16 (name "C")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @3.1-3.20
		(ty-header @3.1-3.4 (name "Foo"))
		(ty-tag-union @3.8-3.17
			(ty-tag-name @3.9-3.10 (name "A"))
			(ty-tag-name @3.12-3.13 (name "B"))
			(ty-tag-name @3.15-3.16 (name "C")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @3.1-3.20 (type "Foo")
			(ty-header @3.1-3.4 (name "Foo"))))
	(expressions))
~~~
