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
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty-tag-union
				(tags
					(ty (name "Whatever"))))
			(associated
				(s-type-decl
					(header (name "Bar")
						(args))
					(ty-tag-union
						(tags
							(ty (name "Something")))))))
		(s-type-anno (name "useBar")
			(ty (name "Foo.Bar")))
		(s-decl
			(p-ident (raw "useBar"))
			(e-tag (raw "Something")))))
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
		(p-assign (ident "useBar"))
		(e-tag (name "Something"))
		(annotation
			(ty-lookup (name "Foo.Bar") (local))))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "Whatever"))))
	(s-nominal-decl
		(ty-header (name "Foo.Bar"))
		(ty-tag-union
			(ty-tag-name (name "Something")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Foo.Bar")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "Foo.Bar")
			(ty-header (name "Foo.Bar"))))
	(expressions
		(expr (type "Foo.Bar"))))
~~~
