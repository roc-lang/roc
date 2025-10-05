# META
~~~ini
description=Qualified names should be checked locally before treating as module references
type=file
~~~
# SOURCE
~~~roc
module []

Foo := [Whatever].{
    Bar := [Something]
}

# This should resolve to the local Foo.Bar, not try to import from a Foo module
useBar : Foo.Bar
useBar = Something
~~~
# EXPECTED
MODULE HEADER DEPRECATED - nominal_associated_vs_module.md:1:1:1:10
TYPE MISMATCH - nominal_associated_vs_module.md:9:10:9:19
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**nominal_associated_vs_module.md:1:1:1:10:**
```roc
module []
```
^^^^^^^^^


**TYPE MISMATCH**
This expression is used in an unexpected way:
**nominal_associated_vs_module.md:9:10:9:19:**
```roc
useBar = Something
```
         ^^^^^^^^^

It has the type:
    _[Something]_others_

But the type annotation says it should have the type:
    _Bar_

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
UpperIdent(3:1-3:4),OpColonEqual(3:5-3:7),OpenSquare(3:8-3:9),UpperIdent(3:9-3:17),CloseSquare(3:17-3:18),Dot(3:18-3:19),OpenCurly(3:19-3:20),
UpperIdent(4:5-4:8),OpColonEqual(4:9-4:11),OpenSquare(4:12-4:13),UpperIdent(4:13-4:22),CloseSquare(4:22-4:23),
CloseCurly(5:1-5:2),
LowerIdent(8:1-8:7),OpColon(8:8-8:9),UpperIdent(8:10-8:13),NoSpaceDotUpperIdent(8:13-8:17),
LowerIdent(9:1-9:7),OpAssign(9:8-9:9),UpperIdent(9:10-9:19),
EndOfFile(10:1-10:1),
~~~
# PARSE
~~~clojure
(file @1.1-9.19
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-decl @3.1-5.2
			(header @3.1-3.4 (name "Foo")
				(args))
			(ty-tag-union @3.8-3.18
				(tags
					(ty @3.9-3.17 (name "Whatever"))))
			(associated @3.19-5.2
				(s-type-decl @4.5-4.23
					(header @4.5-4.8 (name "Bar")
						(args))
					(ty-tag-union @4.12-4.23
						(tags
							(ty @4.13-4.22 (name "Something")))))))
		(s-type-anno @8.1-8.17 (name "useBar")
			(ty @8.10-8.17 (name "Foo.Bar")))
		(s-decl @9.1-9.19
			(p-ident @9.1-9.7 (raw "useBar"))
			(e-tag @9.10-9.19 (raw "Something")))))
~~~
# FORMATTED
~~~roc
module []

Foo := [Whatever].{
	Bar := [Something]
}

# This should resolve to the local Foo.Bar, not try to import from a Foo module
useBar : Foo.Bar
useBar = Something
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @9.1-9.7 (ident "useBar"))
		(e-tag @9.10-9.19 (name "Something"))
		(annotation @9.1-9.7
			(declared-type
				(ty-lookup @8.10-8.17 (name "Foo.Bar") (local)))))
	(s-nominal-decl @3.1-5.2
		(ty-header @3.1-3.4 (name "Foo"))
		(ty-tag-union @3.8-3.18
			(ty-tag-name @3.9-3.17 (name "Whatever"))))
	(s-nominal-decl @4.5-4.23
		(ty-header @4.5-4.8 (name "Bar"))
		(ty-tag-union @4.12-4.23
			(ty-tag-name @4.13-4.22 (name "Something")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @9.1-9.7 (type "Error")))
	(type_decls
		(nominal @3.1-5.2 (type "Foo")
			(ty-header @3.1-3.4 (name "Foo")))
		(nominal @4.5-4.23 (type "Bar")
			(ty-header @4.5-4.8 (name "Bar"))))
	(expressions
		(expr @9.10-9.19 (type "Error"))))
~~~
