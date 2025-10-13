# META
~~~ini
description=Qualified names should be checked locally before treating as module references
type=file
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [Something]
}

# This should resolve to the local Foo.Bar, not try to import from a Foo module
useBar : Foo.Bar
useBar = Something
~~~
# EXPECTED
TYPE MODULE MISSING MATCHING TYPE - nominal_associated_vs_module.md:1:1:7:19
TYPE MISMATCH - nominal_associated_vs_module.md:7:10:7:19
# PROBLEMS
**TYPE MODULE MISSING MATCHING TYPE**
Type modules must have a type declaration matching the module name.

This file is named `nominal_associated_vs_module`.roc, but no top-level type declaration named `nominal_associated_vs_module` was found.

Add either:
`nominal_associated_vs_module := ...` (nominal type)
or:
`nominal_associated_vs_module : ...` (type alias)
**nominal_associated_vs_module.md:1:1:7:19:**
```roc
Foo := [Whatever].{
    Bar := [Something]
}

# This should resolve to the local Foo.Bar, not try to import from a Foo module
useBar : Foo.Bar
useBar = Something
```


# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColonEqual(1:5-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:17),CloseSquare(1:17-1:18),Dot(1:18-1:19),OpenCurly(1:19-1:20),
UpperIdent(2:5-2:8),OpColonEqual(2:9-2:11),OpenSquare(2:12-2:13),UpperIdent(2:13-2:22),CloseSquare(2:22-2:23),
CloseCurly(3:1-3:2),
LowerIdent(6:1-6:7),OpColon(6:8-6:9),UpperIdent(6:10-6:13),NoSpaceDotUpperIdent(6:13-6:17),
LowerIdent(7:1-7:7),OpAssign(7:8-7:9),UpperIdent(7:10-7:19),
EndOfFile(8:1-8:1),
~~~
# PARSE
~~~clojure
(file @1.1-7.19
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-3.2
			(header @1.1-1.4 (name "Foo")
				(args))
			(ty-tag-union @1.8-1.18
				(tags
					(ty @1.9-1.17 (name "Whatever"))))
			(associated @1.19-3.2
				(s-type-decl @2.5-2.23
					(header @2.5-2.8 (name "Bar")
						(args))
					(ty-tag-union @2.12-2.23
						(tags
							(ty @2.13-2.22 (name "Something")))))))
		(s-type-anno @6.1-6.17 (name "useBar")
			(ty @6.10-6.17 (name "Foo.Bar")))
		(s-decl @7.1-7.19
			(p-ident @7.1-7.7 (raw "useBar"))
			(e-tag @7.10-7.19 (raw "Something")))))
~~~
# FORMATTED
~~~roc
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
		(p-assign @7.1-7.7 (ident "useBar"))
		(e-tag @7.10-7.19 (name "Something"))
		(annotation @7.1-7.7
			(declared-type
				(ty-lookup @6.10-6.17 (name "Foo.Bar") (local)))))
	(s-nominal-decl @1.1-3.2
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.18
			(ty-tag-name @1.9-1.17 (name "Whatever"))))
	(s-nominal-decl @2.5-2.23
		(ty-header @2.5-2.23 (name "Foo.Bar"))
		(ty-tag-union @2.12-2.23
			(ty-tag-name @2.13-2.22 (name "Something")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @7.1-7.7 (type "Foo.Bar")))
	(type_decls
		(nominal @1.1-3.2 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo")))
		(nominal @2.5-2.23 (type "Foo.Bar")
			(ty-header @2.5-2.23 (name "Foo.Bar"))))
	(expressions
		(expr @7.10-7.19 (type "Foo.Bar"))))
~~~
